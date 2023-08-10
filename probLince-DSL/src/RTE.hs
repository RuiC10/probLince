module RTE where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Class (lift)
import Cp
import Control.Concurrent
import Data.Time.Clock
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import TrjMonoid
import Numeric.GSL.ODE (odeSolve)
import qualified Numeric.LinearAlgebra as LA
import Exceptions
import Control.Monad.Bayes.Sampler.Lazy (sampler, independent)
import qualified Control.Monad.Bayes.Class as B

data ProbElems = ProbElems {coinList :: [Double], randList :: [Double], normalList :: [Double]}

type Resources = (ProbElems, Map String Double)

type RteM = StateT ((Int, Bool), Resources) (WriterT Trj (ExceptT RunException IO))

readResources :: RteM Resources
readResources = p2 <$> get

stop :: RteM Bool
stop = (p2 . p1) <$> get

readVar :: String -> RteM Double
readVar varName = do
        (l,r) <- p2 <$> get 
        case (Map.lookup varName r) of 
            Nothing -> lift . lift . throwE $ DeclarationException varName
            Just x -> return x

addVar :: String -> Double -> RteM ()
addVar varName value = do
            (l,r) <- p2 <$> get 
            case (Map.lookup varName r) of
                Just _ -> lift . lift . throwE $ AlreadyDeclaredException varName
                Nothing -> do
                        modify (id >< const (l, Map.insert varName value r))
                        return ()

updateVar :: String -> (Double -> Double) -> RteM ()
updateVar varName f = do
            (l,r) <- p2 <$> get 
            case (Map.lookup varName r) of
                Nothing -> lift . lift . throwE $ DeclarationException varName
                (Just _) -> do
                        modify (id >< const (l, Map.insert varName (f (r Map.! varName)) r))
                        return ()

coin :: RteM Double
coin = do
        (l,r) <- p2 <$> get 
        modify (id >< const (l {coinList = (Prelude.tail $ coinList l)}, r))
        return (head . coinList $ l) 

rand :: RteM Double
rand = do
        (l,r) <- p2 <$> get 
        modify (id >< const (l {randList = (Prelude.tail $ randList l)}, r))
        return (head . randList $ l) 

normal :: RteM Double
normal = do
        (l,r) <- p2 <$> get 
        modify (id >< const (l {normalList = (Prelude.tail $ normalList l)}, r))
        return (head . normalList $ l) 

wait :: Int -> RteM ()
wait dur  = do
    ((time, st), (r,m)) <- get
    a <- lift . lift . lift $ getCurrentTime

    if time <= dur then do
        lift . lift . lift . threadDelay $ time
        modify (const (0, False) >< id)

    else do
        lift . lift . lift . threadDelay $ dur
        modify (const (time - dur, True) >< id)

    b <- lift . lift . lift $ getCurrentTime
    lift $ tell ((Trj [(floor $ diffUTCTime b a * 1000000, const m)]) :: Trj)
    return ()

evolve :: Int -> [String] -> (Double -> [Double] -> [Double]) -> RteM ()
evolve dur vars diff = let varValues m = Prelude.map (\k -> m Map.! k) vars
                           diffSol m t = odeSolve diff (varValues m) (LA.linspace 100 (0,t :: Double))
                           finalMemory t m = Map.fromList . zip vars . last . LA.toLists . diffSol m $ (fromIntegral t) / 1000000.0
                           trajectory realTime m = toTrj realTime (\t -> if t == 0 then m else finalMemory t m)
                       in do
                         ((time, st), (r,m)) <- get
                         a <- lift . lift . lift $ getCurrentTime

                         if time <= dur then do
                             lift . lift . lift . threadDelay $ time
                             modify (const (0, False) >< id)

                         else do
                             lift . lift . lift . threadDelay $ dur
                             modify (const (time - dur, True) >< id)

                         b <- lift . lift . lift $ getCurrentTime
                         let realTime = floor $ diffUTCTime b a * 1000000.0
                         lift . tell $ trajectory realTime m
                         modify (id >< const (r, finalMemory realTime m))
                         return ()

check :: String -> (Double -> Bool) -> RteM Bool 
check name f = readVar name >>= return . f 

runRteM :: RteM a -> Double -> IO (Either RunException (Resources, Trj))
runRteM m time = do 
                infCoinList <- sampler . independent . B.uniformD $ [0,1]
                infRandList <- sampler . independent $ B.uniform 0.0 1.0
                infNormalList <- sampler . independent $ B.normal 0.0 1.0
                fmap (id -|- (p2 >< id)) . runExceptT . runWriterT $ execStateT m ((floor $ time * 1000000, True), (ProbElems {coinList = infCoinList, randList = infRandList, normalList = infNormalList}, Map.empty))
