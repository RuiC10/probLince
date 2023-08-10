module Interleave where

import ProbLinceDSL 
import Cp

initial :: PreM ()
initial = addVar' "p0" 100 >> addVar' "p1" 200 >> addVar' "v0" 4 >> addVar' "v1" 5

condTrj :: PreM Bool
condTrj = do
        p_0 <- readVar' "p0" 
        p_1 <- readVar' "p1"
        return $ p_0 > p_1

event1 :: DEvent
event1 = DiffSystem ["p0", "v0"] (\t [p,v] -> [v, -9.8])

event2 :: DEvent
event2 = DiffSystem ["p1", "v1"] (\t [p,v] -> [v,-15])

trajectory :: PreM ()
trajectory = preDEvent (DiffSystem ["p0", "v0", "p1", "v1"] (\t [p_0, v_0, p_1, v_1] -> [v_0, -3, -v_1, 4])) 5

itl :: PreM ()
itl = sequential' initial (interleave' event1 event2 0.01 condTrj trajectory)
        
runItl :: Int -> String -> Double -> Double -> IO ()
runItl numSamples configFile precision dur = (sampleTrj numSamples precision . runPreM dur $ itl) 
                                           >>= trajectoriesToGraphic configFile
