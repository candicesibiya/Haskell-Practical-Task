import Data.List (sortBy)
import Data.Ord (comparing)

-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r


-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18


-- HC1T4 - Task 4: Composing a Function to Process Player Data
extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = [name | (name, _) <- players]

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

topThree :: [(String, Int)] -> [(String, Int)]
topThree players = take 3 players

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

firstN :: Int -> [Int]
firstN n = take n infiniteNumbers


-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers a b = a + b


-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * (5 / 9)


-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


-- Main function to test everything
main :: IO ()
main = do
    putStrLn "=== HC1T1: Function Composition ==="
    print (doubleThenIncrement 5)  -- Expect 11

    putStrLn "\n=== HC1T2: Pure Function Example ==="
    print (circleArea 10)  -- Expect ~314.159

    putStrLn "\n=== HC1T3: Greater than 18 ==="
    print (greaterThan18 20)  -- True
    print (greaterThan18 10)  -- False

    putStrLn "\n=== HC1T4: Top Three Players ==="
    let players = [("Alice",50),("Bob",70),("Eve",65),("Tom",40)]
    print (getTopThreePlayers players)  -- ["Bob","Eve","Alice"]

    putStrLn "\n=== HC1T5: Laziness in Haskell ==="
    print (firstN 10)  -- [1..10]

    putStrLn "\n=== HC1T6: Add Numbers ==="
    print (addNumbers 3 4)  -- 7

    putStrLn "\n=== HC1T7: Fahrenheit to Celsius ==="
    print (fToC 212)  -- 100.0

    putStrLn "\n=== HC1T8: Higher-Order Functions ==="
    print (applyTwice (+3) 5)  -- 11
    print (applyTwice (*2) 4)  -- 16
