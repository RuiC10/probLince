module Interpreter.TestInterp where

import Parser.TestParser
import Interpreter.RTE
import Interpreter.PRE
import Interpreter.DTermInterp
import Util.Cp

testRTE :: Test -> Resources -> RteM (Bool, Resources) 
testRTE (BTerm b) r = return (b,r)
testRTE (Or t1 t2) r = testRTE t1 r >>= (\(b, r') -> ((||) b >< id) <$> testRTE t2 r')
testRTE (And t1 t2) r = testRTE t1 r >>= (\(b, r') -> ((&&) b >< id) <$>  testRTE t2 r')
testRTE (Not t1) r = (not >< id) <$> testRTE t1 r 
testRTE (Gt dt1 dt2) r = dtermRTE dt1 r >>= (\(v, r') -> ((>) v >< id) <$> dtermRTE dt2 r')
testRTE (Lt dt1 dt2) r = dtermRTE dt1 r >>= (\(v, r') -> ((<) v >< id) <$> dtermRTE dt2 r')
testRTE (Eq dt1 dt2) r = dtermRTE dt1 r >>= (\(v, r') -> ((==) v >< id) <$> dtermRTE dt2 r')

testPRE :: Test -> Memory -> PreM Bool 
testPRE (BTerm b) _ = return b
testPRE (Or t1 t2) m = (||) <$> testPRE t1 m <*> testPRE t2 m
testPRE (And t1 t2) m = (&&) <$> testPRE t1 m <*> testPRE t2 m 
testPRE (Not t1) m = not <$> testPRE t1 m
testPRE (Gt dt1 dt2) m = (>) <$> dtermPRE dt1 m <*> dtermPRE dt2 m
testPRE (Lt dt1 dt2) m = (<) <$> dtermPRE dt1 m <*> dtermPRE dt2 m
testPRE (Eq dt1 dt2) m = (==) <$> dtermPRE dt1 m <*> dtermPRE dt2 m
