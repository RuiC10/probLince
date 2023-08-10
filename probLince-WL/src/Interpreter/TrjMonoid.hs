module Interpreter.TrjMonoid where

import Data.Map (Map)
import qualified Data.Map as Map
import Util.Cp

data Trj = Trj [(Int, Int -> Map String Double)]

instance Semigroup Trj where
    Trj t1 <> Trj t2 = Trj (t1 <> t2) 

instance Monoid Trj where
    mempty = Trj []

toTrj :: Int -> (Int -> Map String Double) -> Trj
toTrj dur memoryMap = Trj [(dur, memoryMap)]

getTime :: Trj -> Int
getTime (Trj l) = foldl (\time t -> (time +) . p1 $ t) 0 l

getTrajTrace :: Int -> Int -> (Int, Int -> Map String Double) -> (Int, Map String [(Int, Double)])
getTrajTrace p durNow (dur, trj) = (dur + durNow, Map.unionsWith (++) . Prelude.map (\v -> Map.map ((:[]) . (,) (durNow + v)) . trj $ v) $ [0] ++ [p,2*p..dur-p] ++ [dur])

getMemTrace :: Int -> Trj -> Map String [[(Int, Double)]]
getMemTrace p (Trj l) = Map.unionsWith (++) 
                      . Prelude.map (Map.map (:[])) 
                      . p2 
                      $ Prelude.foldl (memTrajectory p) (0, []) l
    where
        memTrajectory pr (durNow, trjs) newTrj = (id >< (trjs ++) . (:[])) $ getTrajTrace pr durNow newTrj

