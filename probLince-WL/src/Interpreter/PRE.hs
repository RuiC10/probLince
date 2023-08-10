module Interpreter.PRE where

import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler.Lazy 
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.GSL.ODE (odeSolve)
import qualified Numeric.LinearAlgebra as LA
import Util.Cp
import Interpreter.Exceptions
import Interpreter.TrjMonoid 

type Prob = Weighted Sampler 

type Memory = Map String Double

type PreM = StateT (Int, Bool) (WriterT Trj (ExceptT RunException Prob))

addVarPRE :: String -> Memory -> Double -> PreM Memory
addVarPRE varName m v = if not $ Map.member varName m 
                        then return $ Map.insert varName v m
                        else lift . lift . throwE $ AlreadyDeclaredException varName

updateVarPRE :: String -> Memory -> Double -> PreM Memory
updateVarPRE varName m v = if Map.member varName m 
                           then return $ Map.insert varName v m
                           else lift . lift . throwE $ DeclarationException varName

evolvePRE :: Memory -> Int -> (Double -> [Double] -> [Double]) -> PreM Memory
evolvePRE m dur diff = let diffSol t = odeSolve diff (Prelude.map p2 $ Map.toList $ m) (LA.linspace 100 (0,t :: Double))
                           finalMemory t = Map.fromList . zip (Prelude.map p1 $ Map.toList $ m) . last . LA.toLists . diffSol $ (fromIntegral t) / 1000000.0
                           trajectory time = toTrj time (\t -> if t == 0 then m else finalMemory t)
                       in do
                           (time, _) <- get
                           if time <= dur then do
                               put (0, False)
                               lift (tell $ trajectory time) >> return (finalMemory time)
                           else do
                               put (time - dur, True)
                               lift (tell $ trajectory dur) >> return (finalMemory dur)

getVarValuePRE :: String -> Memory -> PreM Double
getVarValuePRE varName m = if Map.member varName m
                           then return $ m Map.! varName
                           else lift . lift . throwE $ DeclarationException varName

waitCallPRE :: Memory -> Int -> PreM Memory
waitCallPRE m dur = do
    (time, _) <- get
    if time <= dur then do
        put (0, False)
        lift . tell $ toTrj time (const m) 
    else do
        put (time - dur, True);
        lift . tell $ toTrj dur (const m)
    return m

runPreMonad :: Double -> PreM Memory -> Prob (Either RunException (Memory, Trj))
runPreMonad time m = runExceptT . runWriterT $ evalStateT m (floor $ time * 1000000, True)
