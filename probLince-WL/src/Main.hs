module Main where

import System.Environment
import Visualization.Histogram (varHist, timeHist)
import Visualization.Graphic
import Interpreter.Stats (sampleTrj, sampleVar, sampleTime, varTrace, timeTrace, trjTrace)
import Interpreter.PRE
import Interpreter.TrjMonoid
import Interpreter.Interpreter (run, sim)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as A
import Data.Map (Map)

rteVarHist :: String -> Either String [(Map String Double, Trj)] -> IO ()
rteVarHist path exec = do
                       content <- A.decode <$> BS.readFile path
                       case content of 
                            Nothing -> putStrLn "Invalid JSON config file" >> helper
                            Just configFile -> either print (varHist configFile) $ varTrace exec

rteTimeHist :: String -> Either String [(Map String Double, Trj)] -> IO ()
rteTimeHist path exec = do
                       content <- A.decode <$> BS.readFile path
                       case content of 
                            Nothing -> putStrLn "Invalid JSON config file" >> helper
                            Just configFile -> either print (timeHist configFile) $ timeTrace exec

rteTrjGraph :: String -> Double -> Either String [(Map String Double, Trj)] -> IO ()
rteTrjGraph path precision exec = do
                       content <- A.decode <$> BS.readFile path
                       case content of 
                            Nothing -> putStrLn "Invalid JSON config file" >> helper
                            Just configFile -> either print (makeGraphTrajectories configFile) $ trjTrace (floor $ precision * 1000000.0) exec

rteCommand :: [String] -> IO()
rteCommand args = let exec dur = run (args !! 1) (read (args !! 4) :: Int) dur
                      configFile = args !! 3
                  in do 
                      if (args !! 2) == "-h" then exec (read (args !! 5) :: Double) >>= rteVarHist configFile
                      else if (args !! 2) == "-t" then exec (read (args !! 5) :: Double) >>= rteTimeHist configFile
                      else if (args !! 2) == "-trj" then exec (read (args !! 6) :: Double) >>= rteTrjGraph configFile (read (args !! 5) :: Double)
                      else putStrLn "Invalid Command" >> helper

preVarHist :: IO (Prob (Either String (Memory, Trj))) -> Int -> String -> IO ()
preVarHist simulation numSamples configFile = do
                            content <- A.decode <$> (BS.readFile configFile)
                            case content of 
                                Nothing -> putStrLn "Invalid JSON config file" >> helper
                                Just conf -> (simulation >>= sampleVar numSamples) >>= either print (varHist conf)

preTimeHist :: IO (Prob (Either String (Memory, Trj))) -> Int -> String -> IO ()
preTimeHist simulation numSamples configFile = do
                            content <- A.decode <$> (BS.readFile configFile)
                            case content of 
                                Nothing -> putStrLn "Invalid JSON config file" >> helper
                                Just conf -> (simulation >>= sampleTime numSamples) >>= either print (timeHist conf)

preTrjGraph :: IO (Prob (Either String (Memory, Trj))) -> Int -> String -> Double -> IO ()
preTrjGraph simulation numSamples configFile precision = do
                            content <- A.decode <$> (BS.readFile configFile)
                            case content of 
                                Nothing -> putStrLn "Invalid JSON config file" >> helper
                                Just conf -> (simulation >>= sampleTrj numSamples precision) >>= either print (makeGraphTrajectories conf)

preCommand :: [String] -> IO()
preCommand args = let numSamples = read (args !! 4) :: Int
                  in do 
                       if (args !! 2) == "-h" 
                       then preVarHist (sim (args !! 1) (read (args !! 5) :: Double)) numSamples (args !! 3)
                       else if (args !! 2) == "-t" 
                       then preTimeHist (sim (args !! 1) (read (args !! 5) :: Double)) numSamples (args !! 3)
                       else if (args !! 2) == "-trj" 
                       then preTrjGraph (sim (args !! 1) (read (args !! 6) :: Double)) numSamples (args !! 3) (read (args !! 5) :: Double) 
                       else putStrLn "Invalid Command" >> helper

helper :: IO ()
helper = do
    putStrLn "Commands:"
    putStrLn "Execute a program and get the histogram of the variables final value:"
    putStrLn "-rte [Program File Path] -h [Histogram Config Path] [Number of executions] [Maximum Duration]\n"
    putStrLn "Execute a program and get the histogram of the execution time of each execution:"
    putStrLn "-rte [Program File Path] -t [Histogram Config Path] [Number of executions] [Maximum Duration]\n"
    putStrLn "Execute a program and get the histogram of all values the variable had during the execution:"
    putStrLn "-rte [Program File Path] -trj [Graphic Config Path] [Number of executions] [Precision] [Trajectory Duration]\n"
    putStrLn "Simulate a program and get the histogram of the final value of each variable for a certain number of samples:"
    putStrLn "-pre [Program File Path] -h [Histogram Config Path] [Number of samples] [Maximum Duration]\n"
    putStrLn "Simulate a program and get the histogram the execution time for certain number of samples:"
    putStrLn "-pre [Program File Path] -t [Histogram Config Path] [Number of samples] [Maximum Duration]\n"
    putStrLn "Simulate a program and get the graphic which shows the trajectories of each variable for a certain number of samples:"
    putStrLn "-pre [Program File Path] -trj [Graphic Config Path] [Number of samples] [Precision] [Trajectory Duration]\n"

main :: IO()
main = do
    args <- getArgs
    if (length args == 6 && (args !! 2) /= "-trj") || (length args == 7 && (args !! 2) == "-trj") then
        if args !! 0 == "-pre" then preCommand args
        else if args !! 0 == "-rte" then rteCommand args
        else print "Invalid Command" >> helper
    else 
        print "Invalid Command" >> helper
