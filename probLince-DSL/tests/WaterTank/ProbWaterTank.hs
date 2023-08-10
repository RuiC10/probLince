module ProbWaterTank where

import ProbLinceDSL

initial :: PreM ()
initial = sequential' (addVar' "waterLevel" 0) signalDuration 

signalDuration :: PreM ()
signalDuration = rand' >>= (\v -> addVar' "signalDuration" (v*0.5 + 2))

condOpen :: PreM Bool
condOpen = do
        l <- readVar' "waterLevel" 
        return $ l < 5

condClose :: PreM Bool
condClose = do
        l <- readVar' "waterLevel" 
        return $ l > 10

signalOpen :: PreM ()
signalOpen = do
        dur <- readVar' "signalDuration" 
        simDEvent (DiffSystem ["waterLevel"] (\t [level] -> [-2])) dur

signalClose :: PreM ()
signalClose = do 
        dur <- readVar' "signalDuration" 
        simDEvent (DiffSystem ["waterLevel"] (\t [level] -> [1])) dur

activatePump :: DEvent
activatePump = DiffSystem ["waterLevel"] (\t [level] -> [1])

deactivatePump :: DEvent
deactivatePump = DiffSystem ["waterLevel"] (\t [level] -> [-2]) 

fillTank :: PreM ()
fillTank = sequential' (untilC' (simDEvent activatePump 0.1) condClose) signalClose

emptyTank :: PreM ()
emptyTank = sequential' (untilC' (simDEvent deactivatePump 0.1) condOpen) signalOpen

waterTank :: PreM ()
waterTank = sequential' initial (infinite' (sequential' fillTank emptyTank))
        
runProbWaterTank :: Int -> String -> Double -> Double -> IO ()
runProbWaterTank numSamples configFile precision dur = (sampleTrj numSamples precision . runPreM dur $ waterTank) 
                                                    >>= trajectoriesToGraphic configFile
