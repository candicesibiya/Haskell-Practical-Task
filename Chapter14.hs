{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.List (group, sort)
import Data.Time.Clock.POSIX (getPOSIXTime)

------------------------------------------------------------
-- HC14T1: Hello, Cabal!
------------------------------------------------------------
hc14t1 :: IO ()
hc14t1 = putStrLn "\n=== HC14T1 ===\nHello, Cabal!"

------------------------------------------------------------
-- HC14T2: Pseudo-random number
------------------------------------------------------------
hc14t2 :: IO ()
hc14t2 = do
    time <- getPOSIXTime
    let num = (floor time `mod` 100) + 1
    putStrLn $ "\n=== HC14T2 ===\nYour random number is: " ++ show num

------------------------------------------------------------
-- HC14T3: Numeric underscores
------------------------------------------------------------
hc14t3 :: IO ()
hc14t3 = do
    let largeNumber = 1_000_000
        budget = 75_000_000
    putStrLn $ "\n=== HC14T3 ===\nLarge Number: " ++ show largeNumber
    putStrLn $ "Budget: " ++ show budget

------------------------------------------------------------
-- HC14T4: TypeApplications
------------------------------------------------------------
readAsInt :: String -> Int
readAsInt s = read @Int s

hc14t4 :: IO ()
hc14t4 = do
    let input = "42"
    putStrLn $ "\n=== HC14T4 ===\nConverted '" ++ input ++ "' to Int: " ++ show (readAsInt input)

------------------------------------------------------------
-- HC14T5: Custom data type and pattern matching with @
------------------------------------------------------------
data Result a = Success a | Failure String deriving Show

processResult :: Result Int -> String
processResult res@(Success x) = "Got " ++ show x ++ " from " ++ show res
processResult res@(Failure msg) = "Error: " ++ msg ++ " (" ++ show res ++ ")"

hc14t5 :: IO ()
hc14t5 = do
    putStrLn "\n=== HC14T5 ==="
    putStrLn $ processResult (Success 99)
    putStrLn $ processResult (Failure "Invalid input")

------------------------------------------------------------
-- HC14T6: Greeting example (self-contained)
------------------------------------------------------------
sayHello :: String -> String
sayHello name = "Hello, " ++ name ++ "!"

hc14t6 :: IO ()
hc14t6 = do
    putStrLn "\n=== HC14T6 ==="
    putStrLn (sayHello "Cabal User")

------------------------------------------------------------
-- HC14T7: sumNonEmpty example
------------------------------------------------------------
sumNonEmpty :: [Int] -> Int
sumNonEmpty [] = error "Cannot sum an empty list!"
sumNonEmpty xs = sum xs

hc14t7 :: IO ()
hc14t7 = do
    let nums = [3,6,9,12]
    putStrLn "\n=== HC14T7 ==="
    putStrLn $ "Numbers: " ++ show nums
    putStrLn $ "Sum using sumNonEmpty: " ++ show (sumNonEmpty nums)

------------------------------------------------------------
-- HC14T8: Character frequency
------------------------------------------------------------
counts :: String -> [(Char, Int)]
counts str = [(head x, length x) | x <- group (sort str)]

hc14t8 :: IO ()
hc14t8 = do
    putStrLn "\n=== HC14T8 ==="
    putStrLn $ "Character counts in 'banana': " ++ show (counts "banana")

------------------------------------------------------------
-- HC14T9: PartialTypeSignatures
------------------------------------------------------------
sumList :: _ -> _
sumList xs = sum xs

hc14t9 :: IO ()
hc14t9 = do
    let nums = [10,20,30]
    putStrLn "\n=== HC14T9 ==="
    putStrLn $ "SumList of [10,20,30]: " ++ show (sumList nums)

------------------------------------------------------------
-- HC14T10: Test-like check for counts
------------------------------------------------------------
hc14t10 :: IO ()
hc14t10 = do
    let testInput = "apple"
        expected = [('a',1),('e',1),('l',1),('p',2)]
    putStrLn "\n=== HC14T10 ==="
    if counts testInput == expected
        then putStrLn "Test passed!"
        else putStrLn "Test failed!"

------------------------------------------------------------
-- Main: run all tasks sequentially
------------------------------------------------------------
main :: IO ()
main = do
    hc14t1
    hc14t2
    hc14t3
    hc14t4
    hc14t5
    hc14t6
    hc14t7
    hc14t8
    hc14t9
    hc14t10
