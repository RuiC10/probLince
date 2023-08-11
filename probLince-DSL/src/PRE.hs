module PRE where

import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler.Lazy 
import Control.Monad.Trans.Class (lift)
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.GSL.ODE (odeSolve)
import qualified Numeric.LinearAlgebra as LA
import Cp
import Exceptions
import TrjMonoid 
import Control.Monad.Bayes.Sampler.Lazy (sampler, independent)
import qualified Control.Monad.Bayes.Class as B

type Prob = Weighted Sampler 

type PreM = StateT ((Int, Bool), Memory) (WriterT Trj (ExceptT RunException Prob))

readMemory :: PreM Memory
readMemory = p2 <$> get

stop' :: PreM Bool
stop' = (p2 . p1) <$> get

readVar' :: String -> PreM Double
readVar' varName = do
                    m <- p2 <$> get 
                    if Map.member varName m 
                    then return $ m Map.! varName  
                    else lift . lift . throwE $ DeclarationException varName

addVar' :: String -> Double -> PreM ()
addVar' varName v = do
                    m <- p2 <$> get 
                    if not $ Map.member varName m 
                    then modify (id >< Map.insert varName v) >> return ()
                    else lift . lift . throwE $ AlreadyDeclaredException varName

updateVar' :: String -> (Double -> Double) -> PreM ()
updateVar' varName f = do
                    m <- p2 <$> get 
                    if Map.member varName m 
                    then modify (id >< Map.insert varName (f (m Map.! varName))) >> return ()
                    else lift . lift . throwE $ AlreadyDeclaredException varName

evolve' :: Int -> [String] -> (Double -> [Double] -> [Double]) -> PreM ()
evolve' dur vars diff = let varValues m = Prelude.map (\k -> m Map.! k) vars
                            diffSol m t = odeSolve diff (varValues m) (LA.linspace 100 (0,t :: Double))
                            finalMemory t m = Map.union (Map.fromList . zip vars . last . LA.toLists . diffSol m $ (fromIntegral t) / 1000000.0) m
                            trajectory time m = toTrj time (\t -> if t == 0 then m else finalMemory t m)
                        in do
                             ((time, st), m) <- get

                             if time <= dur then do
                                 modify (const (0, False) >< id)
                                 lift . tell $ trajectory time m
                                 modify (id >< const (finalMemory time m)) 

                             else do
                                 modify (const (time - dur, True) >< id)
                                 lift . tell $ trajectory dur m
                                 modify (id >< const (finalMemory dur m)) 

                             return ()

wait' :: Int -> PreM ()
wait' dur = do
    ((time, st), m) <- get

    if time <= dur then do
        modify (const (0, False) >< id)
        lift . tell $ toTrj time (const m)

    else do
        modify (const (time - dur, True) >< id);
        lift . tell $ toTrj dur (const m)

    return ()

check' :: String -> (Double -> Bool) -> PreM Bool 
check' name f = readVar' name >>= return . f 

coin' :: PreM Double
coin' = do
        v <- B.uniformD [0.0,1.0]
        return v

rand' :: PreM Double
rand' = do
         v <- B.uniform 0.0 1.0
         return v

normal' :: PreM Double
normal' = do
        v <- B.normal 0.0 1.0 
        return v

runPreM :: Double -> PreM a -> Prob (Either RunException (Memory, Trj))
runPreM time m = fmap (id -|- (p2 >< id)) . runExceptT . runWriterT $ execStateT m ((floor $ time * 1000000, True), Map.empty)
