-- Chapter 17 Practical Tasks: Semigroups and Monoids
-- Fixed for Haskell editors

import Data.Semigroup
import Data.Monoid

----------------------------
-- HC17T2: MyMin / MyMax --
----------------------------
newtype MyMin = MyMin { getMyMin :: Int } deriving (Show)
instance Semigroup MyMin where
    MyMin x <> MyMin y = MyMin (min x y)
instance Monoid MyMin where
    mempty = MyMin maxBound
    mappend = (<>)

newtype MyMax = MyMax { getMyMax :: Int } deriving (Show)
instance Semigroup MyMax where
    MyMax x <> MyMax y = MyMax (max x y)
instance Monoid MyMax where
    mempty = MyMax minBound
    mappend = (<>)

hc17t2 :: IO ()
hc17t2 = do
    putStrLn "\n=== HC17T2 ==="
    let a = MyMin 5
        b = MyMin 3
        c = MyMax 5
        d = MyMax 7
    putStrLn $ "MyMin combine: " ++ show (a <> b)
    putStrLn $ "MyMax combine: " ++ show (c <> d)
    putStrLn $ "MyMin mconcat: " ++ show (mconcat [a, b, MyMin 10])
    putStrLn $ "MyMax mconcat: " ++ show (mconcat [c, d, MyMax 2])

----------------------------
-- HC17T6: maxSeverity   --
----------------------------
data Severity = Low | Medium | High | Critical deriving (Show, Eq, Ord)

instance Semigroup Severity where
    s1 <> s2 = max s1 s2

instance Monoid Severity where
    mempty = Low
    mappend = (<>)

maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

hc17t6 :: IO ()
hc17t6 = do
    putStrLn "\n=== HC17T6 ==="
    let sevList = [Low, Medium, High, Low, Critical, Medium]
    putStrLn $ "Max severity: " ++ show (maxSeverity sevList)

----------------------------
-- HC17T7: multiplyProducts --
----------------------------
newtype ProductInt = ProductInt { getProductInt :: Int } deriving (Show)
instance Semigroup ProductInt where
    ProductInt x <> ProductInt y = ProductInt (x * y)
instance Monoid ProductInt where
    mempty = ProductInt 1
    mappend = (<>)

multiplyProducts :: [ProductInt] -> ProductInt
multiplyProducts = mconcat

hc17t7 :: IO ()
hc17t7 = do
    putStrLn "\n=== HC17T7 ==="
    let prods = [ProductInt 2, ProductInt 3, ProductInt 4]
    putStrLn $ "Multiplication result: " ++ show (getProductInt $ multiplyProducts prods)

----------------------------
-- HC17T8: foldWithSemigroup --
----------------------------
foldWithSemigroup :: Monoid a => [a] -> a
foldWithSemigroup = mconcat

hc17t8 :: IO ()
hc17t8 = do
    putStrLn "\n=== HC17T8 ==="
    let nums = [Sum 2, Sum 3, Sum 5]  -- Sum from Data.Monoid
    putStrLn $ "Folded sum: " ++ show (getSum $ foldWithSemigroup nums)
    let prods = [ProductInt 2, ProductInt 3, ProductInt 4]
    putStrLn $ "Folded product: " ++ show (getProductInt $ foldWithSemigroup prods)

----------------------------
-- Main: run all tasks
----------------------------
main :: IO ()
main = do
    hc17t2
    hc17t6
    hc17t7
    hc17t8
