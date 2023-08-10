module Interpreter.Interpreter where

import Control.Monad.Bayes.Sampler.Lazy (sampler, independent)
import qualified Control.Monad.Bayes.Class as B
import Util.Cp
import Text.Parsec
import Interpreter.ProgramInterp
import Interpreter.RTE
import Interpreter.PRE
import Interpreter.TrjMonoid
import Parser.Parser
import qualified Data.Map as Map
import Control.Monad.Random.Lazy

runLangParser :: String -> IO (Either ParseError Instruction)
runLangParser = fmap (parse parseProg "") . readFile

runExecs :: Instruction -> Int -> Double -> Resources -> IO [Either String (Memory, Trj)]
runExecs _ 0 _ _ = return []
runExecs inst n time r = let m = programRTE inst r
                             errorHandle = return . (:[]) . i1 . show
                             logResult ((r', mem),trj) = ((:) . i2 $ (mem, trj)) <$> runExecs inst (n-1) time (r', Map.empty)
                         in runRteM m time >>= either errorHandle logResult 

run :: String -> Int -> Double -> IO (Either String [(Memory, Trj)])
run inputFile n time = do
                    infCoinList <- evalRandIO $ getRandomRs (0,1)
                    infRandList <- evalRandIO $ getRandomRs (0.0,1.0)
                    infNormalList <- sampler . independent $ B.normal 0.0 1.0
                    parsedProg <- runLangParser inputFile
                    case parsedProg of 
                        Left e -> return . i1 . show $ e
                        Right inst -> sequence <$> runExecs inst n time (ProbElems {coin = infCoinList, rand = infRandList, normal = infNormalList}, Map.empty)

sim :: String -> Double -> IO (Prob (Either String (Memory, Trj)))
sim inputFile time = do
                parsedProg <- runLangParser inputFile
                case parsedProg of 
                    Left e -> return . return . i1 . show $ e
                    Right inst -> return $ (runPreMonad time $ programPRE inst Map.empty) >>= return . (show -|- id)
