module Interpreter.ProgramInterp where

import Parser.Parser
import Parser.TermParser
import Parser.DiffSystemParser
import Parser.TestParser
import Interpreter.RTE
import Interpreter.PRE
import Interpreter.TermInterp
import Interpreter.TestInterp
import Interpreter.DiffSystemInterp
import qualified Data.Map as Map
import Control.Monad.Trans.State.Lazy (get)
import Util.Cp

{- Exec Semantics -}

seqRTE :: Instruction -> Instruction -> Resources -> RteM Resources
seqRTE inst1 inst2 r = do 
                r' <- programRTE inst1 r 
                s <- get 
                case p2 $ s of 
                    True -> programRTE inst2 r'
                    False -> return r'

declRTE :: String -> Term -> Resources -> RteM Resources
declRTE varName t r = termRTE t r >>= uncurry (addVar varName) 

asgRTE :: String -> Term -> Resources -> RteM Resources
asgRTE varName t r = termRTE t r >>= uncurry (updateVar varName)

condRTE :: Test -> Instruction -> Instruction -> Resources -> RteM Resources
condRTE test inst1 inst2 r = testRTE test r >>= cond p1 (programRTE inst1 . p2) (programRTE inst2 . p2)

whRTE :: Test -> Instruction -> Resources -> RteM Resources
whRTE test inst r = testRTE test r >>= (\(b, r') -> cond (const b) (programRTE (Seq inst (Wh test inst))) return r')

waitRTE :: Term -> Instruction -> Resources -> RteM Resources
waitRTE t inst r = termRTE t r >>= uncurry waitCallRTE . ((floor . (* 1000000)) >< id) >>= programRTE inst 

diffRTE :: DiffSystem -> Term -> Resources -> RteM Resources
diffRTE dfs trjDur r = let runTrj f (dur', r') = evolveRTE dur' r' f
                       in diffSystemRTE dfs (Prelude.map p1 . Map.toList . p2 $ r) >>= (\f -> termRTE trjDur r >>= runTrj f . ((floor . (* 1000000)) >< id))

programRTE :: Instruction -> Resources -> RteM Resources
programRTE (Decl name value) r = declRTE name value r
programRTE (Asg name value) r = asgRTE name value r
programRTE (Seq inst1 inst2) r = seqRTE inst1 inst2 r
programRTE (Wh condition instruction) r = whRTE condition instruction r
programRTE (Cond condition (inst1,inst2)) r = condRTE condition inst1 inst2 r
programRTE (Wait term instruction) r = waitRTE term instruction r
programRTE (Diff dfs time) r = diffRTE dfs time r
programRTE _ r = return r

{- Trajectory simulation -}

seqPRE :: Instruction -> Instruction -> Memory -> PreM Memory
seqPRE inst1 inst2 m = do
                m' <- programPRE inst1 m
                r <- get 
                case p2 r of 
                    True -> programPRE inst2 m'
                    False -> return m'

declPRE :: String -> Term -> Memory -> PreM Memory
declPRE varName t m = termPRE t m >>= addVarPRE varName m 

asgPRE :: String -> Term -> Memory -> PreM Memory
asgPRE varName t m = termPRE t m >>= updateVarPRE varName m 

condPRE :: Test -> Instruction -> Instruction -> Memory -> PreM Memory
condPRE test inst1 inst2 m = testPRE test m >>= (\b -> cond (const b) (programPRE inst1) (programPRE inst2) m)

whPRE :: Test -> Instruction -> Memory -> PreM Memory
whPRE test inst m = testPRE test m >>= (\b -> cond (const b) (programPRE (Seq inst (Wh test inst))) return m)

waitPRE :: Term -> Instruction -> Memory -> PreM Memory
waitPRE t inst m = termPRE t m >>= (waitCallPRE m . floor . (* 1000000)) >>= programPRE inst

diffPRE :: DiffSystem -> Term -> Memory -> PreM Memory
diffPRE dfs trjDur m = let runTrj f dur = evolvePRE m dur f
                       in diffSystemPRE dfs (Prelude.map p1 . Map.toList $ m) >>= (\f -> termPRE trjDur m >>= runTrj f . floor . (* 1000000))

programPRE :: Instruction -> Memory -> PreM Memory
programPRE (Decl name value) = declPRE name value
programPRE (Seq inst1 inst2) = seqPRE inst1 inst2 
programPRE (Asg name value) = asgPRE name value
programPRE (Wh condition instruction) = whPRE condition instruction
programPRE (Cond condition (inst1, inst2)) = condPRE condition inst1 inst2
programPRE (Wait term instruction) = waitPRE term instruction
programPRE (Diff dfs time) = diffPRE dfs time
programPRE _ = return
