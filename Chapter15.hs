{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable
import Text.Read (readMaybe)
import System.IO

------------------------------------------------------------
-- HC15T1: Handle Exceptions for File Reading and Velocity
------------------------------------------------------------
hc15t1 :: IO ()
hc15t1 = do
    putStrLn "\n=== HC15T1 ==="
    result <- try (readFile "data.txt") :: IO (Either IOError String)
    case result of
        Left ex -> putStrLn $ "File error: " ++ show ex
        Right content -> putStrLn $ "File content read successfully (first 50 chars): " ++ take 50 content
    let distStr = "100"
        timeStr = "10"
        maybeVelocity = do
            dist <- readMaybe distStr :: Maybe Double
            time <- readMaybe timeStr :: Maybe Double
            if time == 0 then Nothing else Just (dist / time)
    case maybeVelocity of
        Nothing -> putStrLn "Invalid input or division by zero."
        Just v  -> putStrLn $ "Velocity: " ++ show v ++ " m/s"

------------------------------------------------------------
-- HC15T2: Self-Driving AI Car System
------------------------------------------------------------
hc15t2 :: IO ()
hc15t2 = do
    putStrLn "\n=== HC15T2 ==="
    let color = "green"
    case color of
        "red"    -> putStrLn "Stop the car."
        "yellow" -> putStrLn "Slow down the car."
        "green"  -> putStrLn "Go!"
        _        -> putStrLn "Unknown color."

------------------------------------------------------------
-- HC15T3: Custom Exception for Traffic Light Errors
------------------------------------------------------------
data TrafficLightException = TrafficLightException String deriving (Show, Typeable)
instance Exception TrafficLightException

hc15t3 :: IO ()
hc15t3 = do
    putStrLn "\n=== HC15T3 ==="
    let color = "blue"
    catch (if color `elem` ["red","yellow","green"]
            then putStrLn $ "Valid color: " ++ color
            else throwIO $ TrafficLightException ("Invalid traffic light color: " ++ color))
          (\(TrafficLightException msg) -> putStrLn $ "Caught exception: " ++ msg)

------------------------------------------------------------
-- HC15T4: Exception Handler for Traffic Light
------------------------------------------------------------
hc15t4 :: IO ()
hc15t4 = do
    putStrLn "\n=== HC15T4 ==="
    let color = "red"
        handler (TrafficLightException msg) = putStrLn $ "Caught exception: " ++ msg
    catch (if color `elem` ["red","yellow","green"]
            then putStrLn $ "Valid color: " ++ color
            else throwIO $ TrafficLightException ("Invalid traffic light color: " ++ color))
          handler

------------------------------------------------------------
-- HC15T5: Safe Division Using Maybe
------------------------------------------------------------
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

hc15t5 :: IO ()
hc15t5 = do
    putStrLn "\n=== HC15T5 ==="
    let x = 10
        y = 2
    case safeDiv x y of
        Just result -> putStrLn $ "Result: " ++ show result
        Nothing -> putStrLn "Cannot divide by zero."

------------------------------------------------------------
-- HC15T6: Safe Input Parsing with readMaybe
------------------------------------------------------------
hc15t6 :: IO ()
hc15t6 = do
    putStrLn "\n=== HC15T6 ==="
    let input = "123"
    case readMaybe input :: Maybe Int of
        Just n  -> putStrLn $ "You entered: " ++ show n
        Nothing -> putStrLn "Invalid number."

------------------------------------------------------------
-- HC15T7: Velocity Calculation with Optionals and Parsing Handling
------------------------------------------------------------
hc15t7 :: IO ()
hc15t7 = do
    putStrLn "\n=== HC15T7 ==="
    let dStr = "50"
        tStr = "5"
        velocity = do
            d <- readMaybe dStr :: Maybe Double
            t <- readMaybe tStr :: Maybe Double
            if t == 0 then Nothing else Just (d / t)
    case velocity of
        Just v -> putStrLn $ "Velocity: " ++ show v
        Nothing -> putStrLn "Invalid input or division by zero."

------------------------------------------------------------
-- HC15T8: Division with Either for Detailed Errors
------------------------------------------------------------
safeDivEither :: Double -> Double -> Either String Double
safeDivEither _ 0 = Left "Error: division by zero"
safeDivEither x y = Right (x / y)

hc15t8 :: IO ()
hc15t8 = do
    putStrLn "\n=== HC15T8 ==="
    let x = 20
        y = 4
    case safeDivEither x y of
        Right r -> putStrLn $ "Result: " ++ show r
        Left err -> putStrLn err

------------------------------------------------------------
-- HC15T9: Try Function for File IO Exceptions
------------------------------------------------------------
hc15t9 :: IO ()
hc15t9 = do
    putStrLn "\n=== HC15T9 ==="
    result <- try (readFile "data.txt") :: IO (Either IOError String)
    case result of
        Left ex -> putStrLn $ "File error caught: " ++ show ex
        Right content -> putStrLn $ "File read successfully (first 50 chars): " ++ take 50 content

------------------------------------------------------------
-- HC15T10: Hybrid Error Handling with Either and IO
------------------------------------------------------------
hc15t10 :: IO ()
hc15t10 = do
    putStrLn "\n=== HC15T10 ==="
    let dStr = "100"
        tStr = "20"
        parseAndDivide :: Either String Double
        parseAndDivide = do
            d <- maybe (Left "Invalid distance") Right (readMaybe dStr)
            t <- maybe (Left "Invalid time") Right (readMaybe tStr)
            if t == 0 then Left "Division by zero" else Right (d / t)
    case parseAndDivide of
        Right v -> putStrLn $ "Velocity: " ++ show v
        Left err -> putStrLn $ "Error: " ++ err

------------------------------------------------------------
-- Main: Run all tasks sequentially
------------------------------------------------------------
main :: IO ()
main = do
    hc15t1
    hc15t2
    hc15t3
    hc15t4
    hc15t5
    hc15t6
    hc15t7
    hc15t8
    hc15t9
    hc15t10
