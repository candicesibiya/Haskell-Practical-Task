-- HC5T1: Using applyTwice and applying a function three times
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (applyTwice f x)

increment :: Int -> Int
increment n = n + 1

-- HC5T2: Filtering odd numbers
filterOdd :: [Int] -> [Int]
filterOdd xs = filter odd xs

-- HC5T3: Checking for uppercase letters
startsWithUpper :: String -> Bool
startsWithUpper []     = False
startsWithUpper (c:_)  = c `elem` ['A'..'Z']

anyStartsUpper :: [String] -> Bool
anyStartsUpper xs = any startsWithUpper xs

-- HC5T4: Lambda function
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial application
multiplyByFive :: Num a => a -> a
multiplyByFive = (*5)

-- HC5T6: Function composition
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7: Using the $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Point-free style
addFive :: Num a => a -> a
addFive = (+5)

-- HC5T9: Higher-order function to transform a list
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Combine filter, map, any
anySquareGreater50 :: [Int] -> Bool
anySquareGreater50 = any (>50) . map (^2)


-- MAIN: Test all functions
main :: IO ()
main = do
  -- HC5T1
  print (applyThrice increment 5)             -- 8

  -- HC5T2
  print (filterOdd [1..30])                   -- [1,3,5,...,29]

  -- HC5T3
  print (anyStartsUpper ["hello","World","test"]) -- True
  print (anyStartsUpper ["hello","world","test"]) -- False

  -- HC5T4
  print (biggerThan10 5)                      -- False
  print (biggerThan10 15)                     -- True

  -- HC5T5
  print (multiplyByFive 4)                    -- 20
  print (multiplyByFive 10)                   -- 50

  -- HC5T6
  print (evenSquares [1..10])                 -- [4,16,36,64,100]

  -- HC5T7
  print result                                -- 72

  -- HC5T8
  print (addFive 10)                          -- 15
  print (addFive 0)                           -- 5

  -- HC5T9
  print (transformList increment [1,2,3])    -- [3,4,5]

  -- HC5T10
  print (anySquareGreater50 [1,2,3,4,5,6,7]) -- True
  print (anySquareGreater50 [1,2,3,4,5])     -- False
