{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import System.Random
import Data.Maybe
import Data.List

-- HC20T1: safeDivide with Maybe Monad
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- HC20T2: sequenceMaybe for List of Maybe
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence

-- HC20T3: Writer Monad Logging Calculator
type Calc a = Writer [String] a
addLog :: Int -> Int -> Calc Int
addLog x y = writer (x+y, ["Adding " ++ show x ++ " + " ++ show y])

-- HC20T4: countChars with State Monad
countChars :: Char -> String -> Int
countChars c s = execState (mapM_ count s) 0
  where count x = when (x == c) $ modify (+1)

-- HC20T5: Reader Monad for Configurable Greeting
type Config = String
greet :: Reader Config String
greet = do
    name <- ask
    return $ "Hello, " ++ name

-- HC20T6: doubleMonad Combining Maybe and List
doubleMonad :: Maybe a -> [a] -> [a]
doubleMonad mx xs = maybe [] (\x -> [x]) mx >>= \y -> return y

-- HC20T7: findFirst with Either Monad
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "No element found"
findFirst p (x:xs) = if p x then Right x else findFirst p xs

-- HC20T8: Parser Monad for Simple Expressions
newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }
parseChar :: Char -> Parser Char
parseChar c = Parser f
  where f (x:xs) | x == c = Just (c, xs)
        f _ = Nothing

-- HC20T9: replicateMonad with Identity Monad
replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = Identity (replicate n x)

-- HC20T10: Nested StateT and MaybeT Transformer
nestedMonad :: StateT Int (MaybeT IO) ()
nestedMonad = do
    x <- get
    lift $ MaybeT $ return $ if x > 0 then Just () else Nothing

-- HC20T11: randomWalk with State Monad
randomWalk :: State StdGen Int
randomWalk = state $ \g -> let (x, g') = randomR (-1,1) g in (x, g')

-- HC20T12: File Reading with IO Monad
readFileLines :: FilePath -> IO ()
readFileLines path = readFile path >>= mapM_ putStrLn . lines

-- HC20T13: fibonacciMemo with State Monad
fibonacciMemo :: Int -> State (Map Int Integer) Integer
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
    m <- get
    case lookup n m of
        Just val -> return val
        Nothing -> do
            a <- fibonacciMemo (n-1)
            b <- fibonacciMemo (n-2)
            let val = a + b
            modify (insert n val)
            return val

-- HC20T14: mapMFilter Monadic Map-Filter
mapMFilter :: Monad m => (a -> m Bool) -> [a] -> m [a]
mapMFilter p xs = filterM p xs

-- HC20T15: treeSum with Custom Monad
data Tree a = Leaf | Node a (Tree a) (Tree a)
treeSum :: Num a => Tree a -> a
treeSum Leaf = 0
treeSum (Node x l r) = x + treeSum l + treeSum r

-- HC20T16: retryIO with IO Monad
retryIO :: Int -> IO Bool -> IO Bool
retryIO 0 action = action
retryIO n action = action >>= \success -> if success then return True else retryIO (n-1) action

-- HC20T17: validatePassword with Either Monad
validatePassword :: String -> Either String String
validatePassword pwd
    | length pwd < 6 = Left "Password too short"
    | not (any (`elem` ['0'..'9']) pwd) = Left "Password must contain a number"
    | otherwise = Right pwd

-- HC20T18: MaybeT Monad Transformer for User Input
maybeInput :: MaybeT IO String
maybeInput = MaybeT $ do
    putStrLn "Enter a string:"
    input <- getLine
    return $ if null input then Nothing else Just input

-- HC20T19: Writer Monad-based Logging System
logAdd :: Int -> Int -> Writer [String] Int
logAdd x y = writer (x+y, ["Added " ++ show x ++ " and " ++ show y])

-- HC20T20: batchProcessing with Monadic Bind
batchProcessing :: [IO a] -> IO [a]
batchProcessing = sequence

-- Main to demonstrate all
main :: IO ()
main = do
    putStrLn "HC20T1 safeDivide (10 / 2):" >> print (safeDivide 10 2)
    putStrLn "HC20T2 sequenceMaybe [Just 1, Just 2]:" >> print (sequenceMaybe [Just 1, Just 2])
    putStrLn "HC20T3 Writer addLog 5 3:" >> print (runWriter (addLog 5 3))
    putStrLn "HC20T4 countChars 'a' \"banana\":" >> print (countChars 'a' "banana")
    putStrLn "HC20T5 Reader greet \"Alice\":" >> print (runReader greet "Alice")
    putStrLn "HC20T6 doubleMonad (Just 5) [1,2]:" >> print (doubleMonad (Just 5) [1,2])
    putStrLn "HC20T7 findFirst even [1,3,4,5]:" >> print (findFirst even [1,3,4,5])
    putStrLn "HC20T9 replicateMonad 3 7:" >> print (runIdentity (replicateMonad 3 7))
    putStrLn "HC20T15 treeSum example:" >> print (treeSum (Node 5 (Node 3 Leaf Leaf) (Node 2 Leaf Leaf)))
