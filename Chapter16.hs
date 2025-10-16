import Data.Char (toUpper)
import Data.List (nub)

------------------------------------------------------------
-- HC16T1: Reverse a String
------------------------------------------------------------
hc16t1 :: IO ()
hc16t1 = do
    putStrLn "\n=== HC16T1 ==="
    let str = "Haskell"
        reversed = reverse str
    putStrLn $ "Original: " ++ str
    putStrLn $ "Reversed: " ++ reversed

------------------------------------------------------------
-- HC16T2: Palindrome Checker
------------------------------------------------------------
hc16t2 :: IO ()
hc16t2 = do
    putStrLn "\n=== HC16T2 ==="
    let str = "level"
        isPalindrome s = s == reverse s
    putStrLn $ "String: " ++ str
    putStrLn $ "Is palindrome? " ++ show (isPalindrome str)

------------------------------------------------------------
-- HC16T3: Factorial
------------------------------------------------------------
hc16t3 :: IO ()
hc16t3 = do
    putStrLn "\n=== HC16T3 ==="
    let n = 5
        factorial 0 = 1
        factorial k = k * factorial (k-1)
    putStrLn $ "Factorial of " ++ show n ++ " is " ++ show (factorial n)

------------------------------------------------------------
-- HC16T4: Filter Even Numbers
------------------------------------------------------------
hc16t4 :: IO ()
hc16t4 = do
    putStrLn "\n=== HC16T4 ==="
    let nums = [1..10]
        evens = filter even nums
    putStrLn $ "Original list: " ++ show nums
    putStrLn $ "Even numbers: " ++ show evens

------------------------------------------------------------
-- HC16T5: Uppercase String
------------------------------------------------------------
hc16t5 :: IO ()
hc16t5 = do
    putStrLn "\n=== HC16T5 ==="
    let str = "Haskell rocks!"
        upperStr = map toUpper str
    putStrLn $ "Original: " ++ str
    putStrLn $ "Uppercase: " ++ upperStr

------------------------------------------------------------
-- HC16T6: nth Fibonacci Number
------------------------------------------------------------
hc16t6 :: IO ()
hc16t6 = do
    putStrLn "\n=== HC16T6 ==="
    let n = 10
        fib 0 = 0
        fib 1 = 1
        fib k = fib (k-1) + fib (k-2)
    putStrLn $ "Fibonacci number " ++ show n ++ " is " ++ show (fib n)

------------------------------------------------------------
-- HC16T7: Element Existence in List
------------------------------------------------------------
hc16t7 :: IO ()
hc16t7 = do
    putStrLn "\n=== HC16T7 ==="
    let lst = [1,2,3,4,5]
        x = 3
        exists el l = el `elem` l
    putStrLn $ "List: " ++ show lst
    putStrLn $ show x ++ " exists? " ++ show (exists x lst)

------------------------------------------------------------
-- HC16T8: Insertion Sort
------------------------------------------------------------
hc16t8 :: IO ()
hc16t8 = do
    putStrLn "\n=== HC16T8 ==="
    let lst = [5,2,9,1,5,6]
        insert x [] = [x]
        insert x (y:ys)
            | x <= y    = x:y:ys
            | otherwise = y : insert x ys
        insertionSort [] = []
        insertionSort (z:zs) = insert z (insertionSort zs)
    putStrLn $ "Original list: " ++ show lst
    putStrLn $ "Sorted list: " ++ show (insertionSort lst)

------------------------------------------------------------
-- HC16T9: Remove Duplicates from List
------------------------------------------------------------
hc16t9 :: IO ()
hc16t9 = do
    putStrLn "\n=== HC16T9 ==="
    let lst = [1,2,3,2,4,1,5]
        removeDuplicates [] = []
        removeDuplicates (x:xs)
            | x `elem` xs = removeDuplicates xs
            | otherwise = x : removeDuplicates xs
    putStrLn $ "Original list: " ++ show lst
    putStrLn $ "Without duplicates: " ++ show (removeDuplicates lst)

------------------------------------------------------------
-- HC16T10: Character Frequency in String
------------------------------------------------------------
hc16t10 :: IO ()
hc16t10 = do
    putStrLn "\n=== HC16T10 ==="
    let str = "haskell"
        counts s = [(c, length $ filter (==c) s) | c <- nub s]
    putStrLn $ "String: " ++ str
    putStrLn $ "Character frequencies: " ++ show (counts str)

------------------------------------------------------------
-- Main: Run all tasks sequentially
------------------------------------------------------------
main :: IO ()
main = do
    hc16t1
    hc16t2
    hc16t3
    hc16t4
    hc16t5
    hc16t6
    hc16t7
    hc16t8
    hc16t9
    hc16t10
