module Interpreter.RTE where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Class (lift)
import Util.Cp
import Control.Concurrent
import Data.Time.Clock
import Data.Map (Map)
import qualified Data.Map as Map
import Interpreter.TrjMonoid
import Numeric.GSL.ODE (odeSolve)
import qualified Numeric.LinearAlgebra as LA
import Interpreter.Exceptions

data ProbElems = ProbElems {coin :: [Double], rand :: [Double], normal :: [Double]}

type Resources = (ProbElems, Map String Double)

type RteM = StateT (Int, Bool) (WriterT Trj (ExceptT RunException IO))

getVarValue :: String -> Resources -> RteM (Double, Resources)
getVarValue varName (l,r) = 
    case (Map.lookup varName r) of 
        Nothing -> lift . lift . throwE $ DeclarationException varName
        Just x -> return (x, (l,r))

addVar :: String -> Double -> Resources -> RteM Resources
addVar varName value (l,r) = 
    case (Map.lookup varName r) of
        Just _ -> lift . lift . throwE $ AlreadyDeclaredException varName
        Nothing -> do
                return (l, Map.insert varName value r)

updateVar :: String -> Double -> Resources -> RteM Resources
updateVar varName value (l,r) = 
    case (Map.lookup varName r) of
        Nothing -> lift . lift . throwE $ DeclarationException varName
        (Just _) -> do
                return (l, Map.insert varName value r)

getCoinValue :: Resources -> RteM (Double, Resources)
getCoinValue = return . split (Prelude.head . coin . p1) ((\a -> a {coin = (Prelude.tail $ coin a)}) >< id)

getRandValue :: Resources -> RteM (Double, Resources)
getRandValue = return . split (Prelude.head . rand . p1) ((\a -> a {rand = (Prelude.tail $ rand a)}) >< id)

getNormalValue :: Resources -> Double -> Double -> RteM (Double, Resources)
getNormalValue r mean std = return . split ((+ mean) . (* std) . Prelude.head . normal . p1) ((\a -> a {normal = (Prelude.tail $ normal a)}) >< id) $ r

waitCallRTE :: Int -> Resources -> RteM Resources
waitCallRTE dur (r,m) = do
    (time, _) <- get
    a <- lift . lift . lift $ getCurrentTime

    if time <= dur then do
        lift . lift . lift . threadDelay $ time
        put (0, False)

    else do
        lift . lift . lift . threadDelay $ dur
        put (time - dur, True)

    b <- lift . lift . lift $ getCurrentTime
    lift . tell $ toTrj ((floor $ diffUTCTime b a) * 1000000) (const m)
    return (r,m)

evolveRTE :: Int -> Resources -> (Double -> [Double] -> [Double]) -> RteM Resources
evolveRTE dur (r,m) diff = let diffSol t = odeSolve diff (Prelude.map p2 $ Map.toList $ m) (LA.linspace 100 (0,t :: Double))
                               finalMemory t = Map.fromList . zip (Prelude.map p1 $ Map.toList $ m) . last . LA.toLists . diffSol . (/1000000.0) . fromIntegral $ t
                               trajectory realTime = toTrj realTime (\t -> if t == 0 then m else finalMemory t)
                           in do
                                (time, _) <- get
                                a <- lift . lift . lift $ getCurrentTime

                                if time <= dur then do
                                    lift . lift . lift . threadDelay $ time
                                    put (0, False)

                                else do
                                    lift . lift . lift . threadDelay $ dur
                                    put (time - dur, True)

                                b <- lift . lift . lift $ getCurrentTime
                                let realTime = floor $ diffUTCTime b a * 1000000.0
                                lift (tell $ trajectory realTime) >> return (r, finalMemory realTime)

runRteM :: RteM Resources -> Double -> IO (Either RunException (Resources, Trj))
runRteM m time = runExceptT . runWriterT $ evalStateT m (floor $ time * 1000000, True)
