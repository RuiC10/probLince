module Interpreter.DTermInterp where

import Parser.DTermParser
import Interpreter.PRE
import Interpreter.RTE
import Interpreter.Exceptions
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import Util.Cp

-- Semantics

dtermRTE :: DTerm -> Resources -> RteM (Double, Resources)
dtermRTE (Const x) r = return (x,r)
dtermRTE (Var x) r = getVarValue x r
dtermRTE (Add x y) r = dtermRTE x r >>= (\(v, r') -> ((+) v >< id) <$> dtermRTE y r')
dtermRTE (Mult x y) r = dtermRTE x r >>= (\(v, r') -> ((*) v >< id) <$> dtermRTE y r')
dtermRTE (Div x y) r = dtermRTE x r >>= (\(v, r') -> ((/) v >< id) <$> checkZeroDTerm y r')
dtermRTE (Sub x y) r = dtermRTE x r >>= (\(v, r') -> ((-) v >< id) <$> dtermRTE y r')
dtermRTE (Negate x) r = dtermRTE x r >>= return . (negate >< id)

checkZeroDTerm t2 r = dtermRTE t2 r >>= (\(d, r') -> if d == 0 then lift . lift $ throwE ZeroDivisionException else return (d, r'))

dtermPRE :: DTerm -> Memory -> PreM Double
dtermPRE (Const x) _ = return x
dtermPRE (Var x) m = getVarValuePRE x m 
dtermPRE (Add x y) m = (+) <$> dtermPRE x m <*> dtermPRE y m
dtermPRE (Mult x y) m = (*) <$> dtermPRE x m <*> dtermPRE y m
dtermPRE (Div x y) m = (/) <$> dtermPRE x m <*> checkZeroDTermPRE y m
dtermPRE (Sub x y) m = (-) <$> dtermPRE x m <*> dtermPRE y m
dtermPRE (Negate x) m = dtermPRE x m >>= return . negate

checkZeroDTermPRE t2 r = dtermPRE t2 r >>= (\d -> if d == 0 then lift . lift $ throwE ZeroDivisionException else return d)
