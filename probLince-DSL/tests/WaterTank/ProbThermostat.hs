module Thermostat where

import ProbTimeDSL 

initialTemp :: PreM ()
initialTemp = addVar' "temperature" 20

heaterPower :: PreM ()
heaterPower = addVar' "heaterPower" 50

tempMax :: PreM ()
tempMax = normal' >>= (\v -> addVar' "tempMax" (v * 0.1 + 25))

tempMin :: PreM ()
tempMin = normal' >>= (\v -> addVar' "tempMin" (v * 0.1 + 18))

initial :: PreM ()
initial = foldl sequential' initialTemp [tempMin, tempMax, heaterPower] 

condOn :: PreM Bool
condOn = do
        temperature <- readVar' "temperature" 
        minTemp <- readVar' "tempMin" 
        return $ temperature < minTemp

condOff :: PreM Bool
condOff = do
        temperature <- readVar' "temperature" 
        maxTemp <- readVar' "tempMax" 
        return $ temperature > maxTemp

turnOn :: DEvent
turnOn = DiffSystem ["temperature", "heaterPower"] (\t [temperature, heaterPower] -> [0.1 * (heaterPower - temperature), 0])

turnOff :: DEvent
turnOff = DiffSystem ["temperature"] (\t [temperature] -> [negate $ 0.1 * temperature])

heatUp :: PreM ()
heatUp = untilC' (preDEvent turnOn 0.1) condOff

coolDown :: PreM ()
coolDown = untilC' (preDEvent turnOff 0.1) condOn

thermostat :: PreM ()
thermostat = sequential' initial (infinite' (sequential' coolDown heatUp))
        
runThermostat :: Int -> String -> Double -> Double -> IO ()
runThermostat numSamples configFile precision dur = (sampleTrj numSamples precision . runPreM dur $ thermostat) 
                                                  >>= trajectoriesToGraphic configFile
