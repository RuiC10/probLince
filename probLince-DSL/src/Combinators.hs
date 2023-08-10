module Combinators where

import RTE
import PRE
import TrjMonoid
import DEvent
import Cp
import Control.Monad.Writer (foldM_)

sequential :: RteM () -> RteM () -> RteM ()
sequential e1 e2 = do
    e1
    continue <- stop
    if continue then
        e2
    else
        return ()

sequential' :: PreM () -> PreM () -> PreM ()
sequential' e1 e2 = do
    e1
    continue <- stop'
    if continue then
        e2
    else
        return ()

infinite :: RteM () -> RteM ()
infinite s = sequential s (infinite s)

infinite' :: PreM () -> PreM ()
infinite' s = sequential' s (infinite' s)

repeatH :: RteM () -> Int -> RteM ()
repeatH ex 0 = return ()
repeatH ex n = sequential ex (repeatH ex (pred n))

untilC :: RteM () -> RteM Bool -> RteM ()
untilC ex c = c >>= (\b -> if not b then sequential ex (untilC ex c) else return ()) 

conditional :: RteM Bool -> RteM () -> RteM () -> RteM ()
conditional c ex1 ex2 = c >>= (\b -> if b then ex1 else ex2)

after :: Double -> RteM () -> RteM ()
after t e = sequential (rteDEvent Wait t) e

repeatAfter :: Int -> Double -> RteM () -> RteM () 
repeatAfter n t e = after t (repeatH e n)

repeatH' :: PreM () -> Int -> PreM ()
repeatH' ex 0 = return ()
repeatH' ex n = sequential' ex (repeatH' ex (pred n))

untilC' :: PreM () -> PreM Bool -> PreM ()
untilC' ex c = c >>= (\b -> if b then return () else sequential' ex (untilC' ex c))

conditional' :: PreM Bool -> PreM () -> PreM () -> PreM ()
conditional' c ex1 ex2 = c >>= (\b -> if b then ex1 else ex2)

after' :: Double -> PreM () -> PreM ()
after' t e = sequential' (preDEvent Wait t) e

repeatAfter' :: Int -> Double -> PreM () -> PreM ()
repeatAfter' n t e = after' t (repeatH' e n)

-- Event combinators 

sequentialEvent :: DEvent -> DEvent -> Double -> RteM ()
sequentialEvent e1 e2 dur = sequential (rteDEvent e1 dur) (rteDEvent e2 dur)

interleave :: DEvent -> DEvent -> Double -> RteM Bool -> RteM () -> RteM ()
interleave e1 e2 dur c e3 = do
                    b <- c
                    if b then 
                        e3
                    else
                        sequential (sequential (rteDEvent e2 dur) (rteDEvent e1 dur)) (interleave e1 e2 dur c e3)
                    

sequentialEvent' :: DEvent -> DEvent -> Double -> PreM ()
sequentialEvent' e1 e2 dur = sequential' (preDEvent e1 dur) (preDEvent e2 dur)

interleave' :: DEvent -> DEvent -> Double -> PreM Bool -> PreM () -> PreM ()
interleave' e1 e2 dur c e3 = do
                    b <- c
                    if b then 
                        e3
                    else
                        sequential' (sequential' (preDEvent e1 dur) (preDEvent e2 dur)) (interleave' e1 e2 dur c e3)
