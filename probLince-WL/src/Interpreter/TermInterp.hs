module Interpreter.TermInterp where

import Parser.TermParser
import Interpreter.DTermInterp
import Interpreter.RTE
import Interpreter.Exceptions
import Interpreter.PRE
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Util.Cp
import qualified Control.Monad.Bayes.Class as B

termRTE :: Term -> Resources -> RteM (Double, Resources)
termRTE (DetTerm dt) r = dtermRTE dt r
termRTE (COIN) r = getCoinValue r
termRTE (RAND) r = getRandValue r
termRTE (NORMAL _ _) r = getRandValue r
termRTE (AddT t1 t2) r = termRTE t1 r >>= (\(d1, r') -> ((+) d1 >< id) <$> termRTE t2 r')
termRTE (SubT t1 t2) r = termRTE t1 r >>= (\(d1, r') -> ((-) d1 >< id) <$> termRTE t2 r')
termRTE (DivT t1 t2) r = termRTE t1 r >>= (\(d1, r') -> ((/) d1 >< id) <$> checkZero t2 r') 
termRTE (MultT t1 t2) r = termRTE t1 r >>= (\(d1, r') -> ((*) d1 >< id) <$> termRTE t2 r')
termRTE (NegateT t) r = termRTE t r >>= return . (negate >< id)

checkZero t2 r = termRTE t2 r >>= (\(d, r') -> if d == 0 then lift . lift $ throwE ZeroDivisionException else return (d, r'))

termPRE :: Term -> Memory -> PreM Double 
termPRE (DetTerm dt) m = dtermPRE dt m
termPRE (COIN) _ = B.uniformD [0.0, 1.0]
termPRE (RAND) _ = B.uniform 0.0 1.0
termPRE (NORMAL t1 t2) m = termPRE t1 m >>= (\v1 -> termPRE t2 m >>= (\v2 -> ((+ v1) . (v2 *)) <$> B.normal 0 1))
termPRE (AddT t1 t2) m = (+) <$> termPRE t1 m <*> termPRE t2 m
termPRE (SubT t1 t2) m = (-) <$> termPRE t1 m <*> termPRE t2 m
termPRE (DivT t1 t2) m = (/) <$> termPRE t1 m <*> checkZeroPRE t2 m 
termPRE (MultT t1 t2) m = (*) <$> termPRE t1 m <*> termPRE t2 m
termPRE (NegateT t1) m = negate <$> termPRE t1 m

checkZeroPRE t2 r = termPRE t2 r >>= (\d -> if d == 0 then lift . lift $ throwE ZeroDivisionException else return d)
