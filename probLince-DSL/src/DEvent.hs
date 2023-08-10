module DEvent where

import RTE
import PRE
import TrjMonoid
import Cp

data DEvent = Wait | DiffSystem [String] (Double -> [Double] -> [Double])

execDEvent :: DEvent -> Double -> RteM ()
execDEvent Wait dur = wait (floor $ dur * 1000000)
execDEvent (DiffSystem vars f) dur = evolve (floor $ dur * 1000000) vars f

simDEvent :: DEvent -> Double -> PreM ()
simDEvent Wait dur = wait' (floor $ dur * 1000000) 
simDEvent (DiffSystem vars f) dur = evolve' (floor $ dur * 1000000) vars f
