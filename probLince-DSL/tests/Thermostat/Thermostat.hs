module Thermostat where

import ProbLinceDSL

initialTemp :: Double
initialTemp = 20

heaterPower :: Double
heaterPower = 50

tempMax :: Double
tempMax = 25

tempMin :: Double
tempMin = 18

initial :: PreM ()
initial = addVar' "temperature" initialTemp

condOn :: PreM Bool
condOn = do
        temperature <- readVar' "temperature" 
        return $ temperature < tempMin

condOff :: PreM Bool
condOff = do
        temperature <- readVar' "temperature" 
        return $ temperature > tempMax

turnOn :: DEvent
turnOn = DiffSystem ["temperature"] (\t [temperature] -> [0.1*(heaterPower - temperature)])

turnOff :: DEvent
turnOff = DiffSystem ["temperature"] (\t [temperature] -> [negate $ 0.1 * temperature])

heatUp :: PreM ()
heatUp = untilC' (simDEvent turnOn 0.01) condOff

coolDown :: PreM ()
coolDown = untilC' (simDEvent turnOff 0.01) condOn

thermostat :: PreM ()
thermostat = sequential' initial (infinite' (sequential' coolDown heatUp))
        
runThermostat :: Int -> String -> Double -> Double -> IO ()
runThermostat numSamples configFile precision dur = (sampleTrj numSamples precision . runPreM dur $ thermostat) 
                                                  >>= trajectoriesToGraphic configFile
