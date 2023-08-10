module WaterTank where

import ProbLinceDSL

initial :: PreM ()
initial = addVar' "waterLevel" 0

condOpen :: PreM Bool
condOpen = do
        l <- readVar' "waterLevel" 
        return $ l < 5

condClose :: PreM Bool
condClose = do
        l <- readVar' "waterLevel" 
        return $ l > 10

signalOpen :: DEvent
signalOpen = DiffSystem ["waterLevel"] (\t [level] -> [-2])

signalClose :: DEvent
signalClose = DiffSystem ["waterLevel"] (\t [level] -> [1])

activatePump :: DEvent
activatePump = DiffSystem ["waterLevel"] (\t [level] -> [1])

deactivatePump :: DEvent
deactivatePump = DiffSystem ["waterLevel"] (\t [level] -> [-2]) 

fillTank :: PreM ()
fillTank = sequential' (untilC' (simDEvent activatePump 0.1) condClose) (simDEvent signalClose 2)

emptyTank :: PreM ()
emptyTank = sequential' (untilC' (simDEvent deactivatePump 0.1) condOpen) (simDEvent signalOpen 2)

waterTank :: PreM ()
waterTank = sequential' initial (infinite' (sequential' fillTank emptyTank))
        
runWaterTank :: Int -> String -> Double -> Double -> IO ()
runWaterTank numSamples configFile precision dur = (sampleTrj numSamples precision . runPreM dur $ waterTank) 
                                                >>= trajectoriesToGraphic configFile
