-- Chapter 18 Practical Tasks: Functors and fmap
-- All tasks combined in one file

import Data.Char (toLower)

----------------------------
-- HC18T1: mapToLower
----------------------------
mapToLower :: [Char] -> [Char]
mapToLower = fmap toLower

hc18t1 :: IO ()
hc18t1 = do
    putStrLn "\n=== HC18T1 ==="
    let str = "Hello World!"
    putStrLn $ "Lowercase: " ++ mapToLower str

----------------------------
-- HC18T2: Functor instance for Tree
----------------------------
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

----------------------------
-- HC18T3: incrementTreeValues
----------------------------
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

hc18t2_3 :: IO ()
hc18t2_3 = do
    putStrLn "\n=== HC18T2 & HC18T3 ==="
    let tree = Node (Leaf 1) 2 (Leaf 3)
    putStrLn $ "Original tree: " ++ show tree
    putStrLn $ "Incremented tree: " ++ show (incrementTreeValues tree)

----------------------------
-- HC18T4: mapToBits
----------------------------
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

hc18t4 :: IO ()
hc18t4 = do
    putStrLn "\n=== HC18T4 ==="
    let bools = [True, False, True, True, False]
    putStrLn $ "Bits: " ++ mapToBits bools

----------------------------
-- HC18T5: Functor instance for MyEither
----------------------------
data MyEither a b = MyLeft a | MyRight b deriving (Show)

instance Functor (MyEither a) where
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight y) = MyRight (f y)

hc18t5 :: IO ()
hc18t5 = do
    putStrLn "\n=== HC18T5 ==="
    let val1 = MyLeft "Error"   :: MyEither String Int
        val2 = MyRight 10       :: MyEither String Int
    print $ fmap (+1) val1
    print $ fmap (+1) val2

----------------------------
-- HC18T6: applyToMaybe
----------------------------
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

hc18t6 :: IO ()
hc18t6 = do
    putStrLn "\n=== HC18T6 ==="
    print $ applyToMaybe (+1) (Just 5)
    print $ applyToMaybe (+1) Nothing

----------------------------
-- HC18T7: fmapTuple
----------------------------
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

hc18t7 :: IO ()
hc18t7 = do
    putStrLn "\n=== HC18T7 ==="
    print $ fmapTuple (+1) ("Count", 10)

----------------------------
-- HC18T8: identityLawCheck
----------------------------
identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == x

hc18t8 :: IO ()
hc18t8 = do
    putStrLn "\n=== HC18T8 ==="
    print $ identityLawCheck (Just 5 :: Maybe Int)
    print $ identityLawCheck ([1,2,3] :: [Int])

----------------------------
-- HC18T9: compositionLawCheck
----------------------------
compositionLawCheck :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

hc18t9 :: IO ()
hc18t9 = do
    putStrLn "\n=== HC18T9 ==="
    print $ compositionLawCheck (+1) (*2) (Just 3)
    print $ compositionLawCheck (+1) (*2) [1,2,3]

----------------------------
-- HC18T10: nestedFmap
----------------------------
nestedFmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
nestedFmap = fmap . fmap

hc18t10 :: IO ()
hc18t10 = do
    putStrLn "\n=== HC18T10 ==="
    let nestedList = [[1,2,3],[4,5,6]]
    print $ nestedFmap (+1) nestedList

----------------------------
-- Main: run all tasks
----------------------------
main :: IO ()
main = do
    hc18t1
    hc18t2_3
    hc18t4
    hc18t5
    hc18t6
    hc18t7
    hc18t8
    hc18t9
    hc18t10
