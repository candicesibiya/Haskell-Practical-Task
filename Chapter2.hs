
import Data.List (sortBy)
import Data.Ord (comparing)

-- HC2T1: Checking Types
exprInt :: Int
exprInt = 42

exprDouble :: Double
exprDouble = 3.14

exprString :: String
exprString = "Haskell"

exprChar :: Char
exprChar = 'Z'

exprBool :: Bool
exprBool = True && False

-- HC2T2: Function Type Signatures
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- HC2T3: Immutable Variables
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T4: Infix vs Prefix Notation
prefix1 = (+) 5 3      
prefix2 = (*) 10 4     
prefix3 = (&&) True False  

infix1 = 7 + 2         
infix2 = 6 * 5         
infix3 = True && False 

-- HC2T5: Defining Functions
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- HC2T6: Int vs Integer
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

-- HC2T7: Boolean Expressions
expr1 :: Bool
expr1 = True && (5 > 3)

expr2 :: Bool
expr2 = False || (3 > 10)

expr3 :: Bool
expr3 = not False

expr4 :: Bool
expr4 = 7 == 10


-- MAIN: Run all tasks
main :: IO ()
main = do
    putStrLn "=== HC2T1: Checking Types ==="
    print exprInt
    print exprDouble
    print exprString
    print exprChar
    print exprBool

    putStrLn "\n=== HC2T2: Functions ==="
    print (add 5 7)
    print (isEven 10)
    print (isEven 13)
    print (concatStrings "Hello, " "Haskell!")

    putStrLn "\n=== HC2T3: Immutable Variables ==="
    print myAge
    print piValue
    print greeting
    print isHaskellFun

    putStrLn "\n=== HC2T4: Infix vs Prefix ==="
    print prefix1
    print prefix2
    print prefix3
    print infix1
    print infix2
    print infix3

    putStrLn "\n=== HC2T5: Functions circleArea & maxOfThree ==="
    print (circleArea 5)
    print (circleArea 2.5)
    print (maxOfThree 10 20 15)
    print (maxOfThree 7 3 9)

    putStrLn "\n=== HC2T6: Int vs Integer ==="
    print smallNumber
    print bigNumber
    putStrLn "Note: 2^64 :: Int will overflow; 2^64 :: Integer works fine in GHCi"

    putStrLn "\n=== HC2T7: Boolean Expressions ==="
    print expr1
    print expr2
    print expr3
    print expr4
