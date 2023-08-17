module Interpreter.Stats where

import Util.Cp
import Data.Map (Map)
import qualified Data.Map as Map
import Interpreter.TrjMonoid
import Interpreter.PRE
import Control.Monad.Bayes.Inference.Lazy.MH (mh)

timeTrace :: Either String [(Map String Double, Trj)] -> Either String [Double]
timeTrace = id -|- Prelude.map ((/1000000.0) . fromIntegral . getTime . p2)

varTrace :: Either String [(Map String Double, Trj)] -> Either String (Map String [Double])
varTrace = id -|- Map.unionsWith (++) . Prelude.map (Map.map (:[]) . p1)

trjTrace :: Int -> Either String [(Map String Double, Trj)] -> Either String [Map String [[(Int, Double)]]]
trjTrace p = id -|- Prelude.map (getMemTrace p . p2)

sampleVar :: Int -> Prob (Either String (Memory, Trj)) -> IO (Either String (Map String [Double]))
sampleVar n dist = (id -|- Map.unionsWith (++) . Prelude.map (Map.map (\x -> [x]) . p1))
                 . sequence
                 . Prelude.map p1
                 . take n
                <$> mh 1.0 dist

sampleTime :: Int -> Prob (Either String (Memory, Trj)) -> IO (Either String [Double])
sampleTime n dist = (id -|- Prelude.map ((/1000000.0) . fromIntegral . getTime . p2))
                  . sequence 
                  . Prelude.map p1
                  . take n
                  <$> mh 1.0 dist

sampleTrj :: Int -> Double -> Prob (Either String (Memory, Trj)) -> IO (Either String ([Map String [[(Int, Double)]]]))
sampleTrj n precision p = (id -|- Prelude.map (getMemTrace (floor $ precision * 1000000) . p2)) . sequence . Prelude.map p1 . take n <$> mh 1.0 p 
