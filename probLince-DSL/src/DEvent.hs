module DEvent where

import RTE
import PRE
import TrjMonoid
import Cp

data DEvent = Wait | DiffSystem [String] (Double -> [Double] -> [Double])

rteDEvent :: DEvent -> Double -> RteM ()
rteDEvent Wait dur = wait (floor $ dur * 1000000)
rteDEvent (DiffSystem vars f) dur = evolve (floor $ dur * 1000000) vars f

preDEvent :: DEvent -> Double -> PreM ()
preDEvent Wait dur = wait' (floor $ dur * 1000000) 
preDEvent (DiffSystem vars f) dur = evolve' (floor $ dur * 1000000) vars f
