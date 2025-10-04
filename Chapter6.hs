-- Haskell Chapter 6 Practical Tasks: Recursion and List Processing

-- =========================
-- HC6T1: Factorial (Recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- =========================
-- HC6T2: Fibonacci (Recursive)
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- =========================
-- HC6T3: Sum of Elements Using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- =========================
-- HC6T4: Product of Elements Using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- =========================
-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- =========================
-- HC6T6: Element Exists in List
elementExists :: (Eq a) => a -> [a] -> Bool
elementExists _ [] = False
elementExists n (x:xs)
  | n == x    = True
  | otherwise = elementExists n xs

-- =========================
-- HC6T7: List Length
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- =========================
-- HC6T8: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven xs = [x | x <- xs, even x]

-- =========================
-- HC6T9: Map Implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- =========================
-- HC6T10: Digits of a Number (Recursive)
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- =========================
-- MAIN: Test all tasks
main :: IO ()
main = do
  -- HC6T1
  putStrLn "\nHC6T1: Factorial of 5"
  print (factorial 5)

  -- HC6T2
  putStrLn "\nHC6T2: 10th Fibonacci number"
  print (fibonacci 10)

  -- HC6T3
  putStrLn "\nHC6T3: Sum of [1,2,3,4,5]"
  print (sumList [1,2,3,4,5])

  -- HC6T4
  putStrLn "\nHC6T4: Product of [1,2,3,4,5]"
  print (productList [1,2,3,4,5])

  -- HC6T5
  putStrLn "\nHC6T5: Reverse [1,2,3,4,5]"
  print (reverseList [1,2,3,4,5])

  -- HC6T6
  putStrLn "\nHC6T6: Check if 3 exists in [1,2,3,4,5]"
  print (elementExists 3 [1,2,3,4,5])

  -- HC6T7
  putStrLn "\nHC6T7: Length of [10,20,30,40]"
  print (listLength [10,20,30,40])

  -- HC6T8
  putStrLn "\nHC6T8: Even numbers from [1..10]"
  print (filterEven [1..10])

  -- HC6T9
  putStrLn "\nHC6T9: Apply (*2) to [1,2,3,4]"
  print (myMap (*2) [1,2,3,4])

  -- HC6T10
  putStrLn "\nHC6T10: Digits of 12345"
  print (digits 12345)
