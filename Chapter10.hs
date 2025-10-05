-- Haskell Chapter 10 Practical Tasks: Custom Type Classes and Instances
-- All tasks combined in one file

-- HC10T1: ShowSimple Type Class
class ShowSimple a where
  showSimple :: a -> String

data PaymentMethod = Cash | CreditCard | PayPal deriving (Show)

instance ShowSimple PaymentMethod where
  showSimple Cash        = "Payment Method: Cash"
  showSimple CreditCard  = "Payment Method: Credit Card"
  showSimple PayPal      = "Payment Method: PayPal"

-- HC10T2: Summable Type Class
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp xs = foldr (+) 0 xs

-- HC10T3: Comparable Type Class
data Blockchain = Bitcoin | Ethereum | Solana deriving (Show)

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith Bitcoin Bitcoin   = EQ
  compareWith Ethereum Ethereum = EQ
  compareWith Solana Solana     = EQ
  compareWith Bitcoin _         = LT
  compareWith Ethereum Bitcoin  = GT
  compareWith Ethereum _        = LT
  compareWith Solana _          = GT

-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving (Show)

instance Eq a => Eq (Box a) where
  Empty == Empty = True
  (Has x) == (Has y) = x == y
  _ == _ = False

-- HC10T5: ShowDetailed Type Class
data User = User { name :: String, age :: Int } deriving (Show)

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User n a) = "User Name: " ++ n ++ ", Age: " ++ show a

-- HC10T6: Mutual Recursion in Eq for Blockchain
instance Eq Blockchain where
  (==) Bitcoin Bitcoin     = True
  (==) Ethereum Ethereum   = True
  (==) Solana Solana       = True
  (==) _ _                 = False
  (/=) a b = not (a == b)

-- HC10T7: Convertible Type Class
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash       = "Cash"
  convert CreditCard = "Credit Card"
  convert PayPal     = "PayPal"

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool
  compareEquality x y = x == y

instance AdvancedEq Int where
  compareEquality x y = x == y

-- HC10T9: MinMax Type Class
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable String where
  concatWith = (++)

-- MAIN FUNCTION TO DEMONSTRATE ALL TASKS
main :: IO ()
main = do
  putStrLn "=== HC10T1: ShowSimple ==="
  print (showSimple Cash)
  print (showSimple CreditCard)
  print (showSimple PayPal)

  putStrLn "\n=== HC10T2: Summable ==="
  print (sumUp [1,2,3,4,5] :: Int)
  print (sumUp [10,20,30] :: Int)

  putStrLn "\n=== HC10T3: Comparable ==="
  print (compareWith Bitcoin Ethereum)
  print (compareWith Solana Ethereum)

  putStrLn "\n=== HC10T4: Eq Instance for Box ==="
  print (Has 5 == Has 5)
  print (Has 3 == Has 7)
  print (Empty == Has 1)

  putStrLn "\n=== HC10T5: ShowDetailed ==="
  let u = User "Sbahle" 21
  print (showDetailed u)

  putStrLn "\n=== HC10T6: Eq Mutual Recursion ==="
  print (Bitcoin == Bitcoin)
  print (Ethereum /= Solana)

  putStrLn "\n=== HC10T7: Convertible ==="
  print (convert Cash :: String)
  print (convert CreditCard :: String)
  print (convert PayPal :: String)

  putStrLn "\n=== HC10T8: AdvancedEq ==="
  print (compareEquality (5 :: Int) 5)
  print (compareEquality (3 :: Int) 7)

  putStrLn "\n=== HC10T9: MinMax ==="
  print (minValue :: Int)
  print (maxValue :: Int)

  putStrLn "\n=== HC10T10: Concatenatable ==="
  print (concatWith "Smart" "CampusPortal")
  print (concatWith "Hello, " "World!")
