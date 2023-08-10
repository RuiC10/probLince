module Interpreter.DiffSystemInterp where

import Parser.DiffSystemParser
import Interpreter.PRE
import Interpreter.RTE
import qualified Data.Map as Map
import qualified Data.Sequence as S
import Data.List (elemIndex)
import Interpreter.Exceptions
import Data.Foldable (toList)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)


diffSystemRTE :: DiffSystem -> [String] -> RteM (Double -> [Double] -> [Double])
diffSystemRTE (DiffSystem eqs) vars = Prelude.foldl (interpDiffEq vars) (return (\_ initVal -> Prelude.map (const 0.0) initVal)) (Map.toList eqs)
    where
        interpDiffEq v system (var, t) = case elemIndex var v of
                                            Nothing -> lift . lift . throwE $ DiffNotDeclaredException var 
                                            Just i -> system >>= (\f -> diffTermRTE v t >>= upd i f)   
        upd i f g = return (\t initVal -> toList . S.update i (g t initVal) . S.fromList $ f t initVal)

diffTermRTE :: [String] -> DiffTerm -> RteM (Double -> [Double] -> Double)
diffTermRTE vars (DTVar v) = findVarRTE v vars >>= (\index -> return (\_ initVals -> (initVals !! index)))
diffTermRTE _ (DTConst d) = return (\_ _ -> d)
diffTermRTE _ DTTime = return (\time _ -> time)
diffTermRTE vars (MultDT t1 t2) = diffTermRTE vars t1 >>= (\f -> diffTermRTE vars t2 >>= (\g -> mergeDiffInterpRTE (*) f g))
diffTermRTE vars (SubDT t1 t2) = diffTermRTE vars t1 >>= (\f -> diffTermRTE vars t2 >>= (\g -> mergeDiffInterpRTE (-) f g))
diffTermRTE vars (SumDT t1 t2) = diffTermRTE vars t1 >>= (\f -> diffTermRTE vars t2 >>= (\g -> mergeDiffInterpRTE (+) f g))
diffTermRTE vars (DivDT t1 t2) = diffTermRTE vars t1 >>= (\f -> diffTermRTE vars t2 >>= (\g -> mergeDiffInterpRTE (/) f g))
diffTermRTE vars (NegateDT t) = diffTermRTE vars t >>= (\f -> return (\time initVal -> negate $ f time initVal))

findVarRTE :: String -> [String] -> RteM Int
findVarRTE var vars = case elemIndex var vars of
                            Nothing -> lift . lift . throwE $ DiffNotDeclaredException var 
                            Just i -> return i 

mergeDiffInterpRTE :: (Double -> Double -> Double) -> (Double -> [Double] -> Double) -> (Double -> [Double] -> Double) -> RteM (Double -> [Double] -> Double)
mergeDiffInterpRTE op f g = return (\time initVal -> op (g time initVal) (f time initVal))

diffSystemPRE :: DiffSystem -> [String] -> PreM (Double -> [Double] -> [Double])
diffSystemPRE (DiffSystem eqs) vars = Prelude.foldl (interpDiffEq vars) (return (\_ initVal -> Prelude.map (const 0.0) initVal)) (Map.toList eqs)
    where
        interpDiffEq v system (var, t) = case elemIndex var v of
                                                Nothing -> lift . lift . throwE $ DiffNotDeclaredException var
                                                Just i -> system >>= (\f -> diffTermPRE v t >>= upd i f)   
        upd i f g = return (\t initVal -> toList . S.update i (g t initVal) . S.fromList $ f t initVal)

diffTermPRE :: [String] -> DiffTerm -> PreM (Double -> [Double] -> Double)
diffTermPRE vars (DTVar v) = findVarPRE v vars >>= (\index -> return (\_ initVals -> (initVals !! index)))
diffTermPRE _ (DTConst d) = return (\_ _ -> d)
diffTermPRE _ DTTime = return (\time _ -> time)
diffTermPRE vars (MultDT t1 t2) = diffTermPRE vars t1 >>= (\f -> diffTermPRE vars t2 >>= (\g -> mergeDiffInterpPRE (*) f g))
diffTermPRE vars (SubDT t1 t2) = diffTermPRE vars t1 >>= (\f -> diffTermPRE vars t2 >>= (\g -> mergeDiffInterpPRE (-) f g))
diffTermPRE vars (SumDT t1 t2) = diffTermPRE vars t1 >>= (\f -> diffTermPRE vars t2 >>= (\g -> mergeDiffInterpPRE (+) f g))
diffTermPRE vars (DivDT t1 t2) = diffTermPRE vars t1 >>= (\f -> diffTermPRE vars t2 >>= (\g -> mergeDiffInterpPRE (/) f g))
diffTermPRE vars (NegateDT t) = diffTermPRE vars t >>= (\f -> return (\time initVal -> negate $ f time initVal))

findVarPRE :: String -> [String] -> PreM Int
findVarPRE var vars = case elemIndex var vars of
                            Nothing -> lift . lift . throwE $ DiffNotDeclaredException var 
                            Just i -> return i 

mergeDiffInterpPRE :: (Double -> Double -> Double) -> (Double -> [Double] -> Double) -> (Double -> [Double] -> Double) -> PreM (Double -> [Double] -> Double)
mergeDiffInterpPRE op f g = return (\time initVal -> op (g time initVal) (f time initVal))
