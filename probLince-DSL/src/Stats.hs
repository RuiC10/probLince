module Stats where

import Cp
import Data.Map (Map)
import qualified Data.Map as Map
import Data.DList as DList
import TrjMonoid
import PRE
import RTE
import Exceptions
import Control.Monad.Bayes.Inference.Lazy.MH (mh)

timeTrace :: Either String [(Map String Double, Trj)] -> Either String [Double]
timeTrace = id -|- Prelude.map ((/1000000.0) . fromIntegral . getTime . p2)

varTrace :: Either String [(Map String Double, Trj)] -> Either String (Map String [Double])
varTrace = id -|- Map.unionsWith (++) . Prelude.map (Map.map (:[]) . p1)

trjTrace :: Int -> Either String [(Map String Double, Trj)] -> Either String [Map String [[(Int, Double)]]]
trjTrace p = id -|- Prelude.map (getMemTrace p . p2)

sampleVar :: Int -> Meas (Either RunException (Memory, Trj)) -> IO (Either String (Map String [Double]))
sampleVar n dist = (show -|- Map.unionsWith (++) . Prelude.map (Map.map (\x -> [x]) . p1))
                 . sequence
                 . Prelude.map p1
                 . take n
                <$> mh 1.0 dist

sampleTime :: Int -> Meas (Either RunException (Memory, Trj)) -> IO (Either String [Double])
sampleTime n dist = (show -|- Prelude.map ((/1000000) . fromIntegral . getTime . p2))
                  . sequence 
                  . Prelude.map p1
                  . take n
                  <$> mh 1.0 dist

sampleTrj :: Int -> Double -> Meas (Either RunException (Memory, Trj)) -> IO (Either String ([Map String [[(Int, Double)]]]))
sampleTrj n precision measure = (show -|- Prelude.map (getMemTrace (floor $ precision * 1000000) . p2)) 
                              . sequence 
                              . Prelude.map p1 
                              . take n 
                              <$> mh 1.0 measure 
