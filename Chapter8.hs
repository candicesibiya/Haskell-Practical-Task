-- Haskell Chapter 8 Practical Tasks: Data Types, Synonyms, and Records

-- =========================
-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to value = "From: " ++ from ++ ", To: " ++ to ++ ", Value: " ++ show value

-- =========================
-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency
  deriving (Show)

data Person1 = Person1
  { name1 :: String
  , address1 :: (String, Int)
  , payment :: PaymentMethod
  } deriving (Show)

bob :: Person1
bob = Person1 { name1 = "Bob", address1 = ("Main Street", 101), payment = Cash }

-- =========================
-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float
  deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- =========================
-- HC8T4: Record Syntax for Employee
data Employee = Employee
  { empName :: String
  , experienceInYears :: Float
  } deriving (Show)

richard :: Employee
richard = Employee { empName = "Richard", experienceInYears = 7.5 }

-- =========================
-- HC8T5: Record Syntax for Person
data Person2 = Person2
  { personName :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving (Show)

person1 :: Person2
person1 = Person2 { personName = "Alice", age = 30, isEmployed = True }

person2 :: Person2
person2 = Person2 { personName = "John", age = 25, isEmployed = False }

-- =========================
-- HC8T6: Record Syntax for Shape Variants
data CircleShape = CircleShape
  { center :: (Float, Float)
  , radius :: Float
  , colorC :: String
  } deriving (Show)

data RectangleShape = RectangleShape
  { width :: Float
  , height :: Float
  , colorR :: String
  } deriving (Show)

myCircle :: CircleShape
myCircle = CircleShape { center = (0,0), radius = 5, colorC = "Red" }

myRectangle :: RectangleShape
myRectangle = RectangleShape { width = 10, height = 5, colorR = "Blue" }

-- =========================
-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String
  deriving (Show)

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dog1 :: Animal
dog1 = Dog "Rex"

cat1 :: Animal
cat1 = Cat "Whiskers"

-- =========================
-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello " ++ name ++ "! You are " ++ show age ++ " years old."

-- =========================
-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving (Show)

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr val =
  let tx = Transaction { from = fromAddr, to = toAddr, amount = val, transactionId = "TX12345" }
  in transactionId tx

-- =========================
-- HC8T10: Deriving Show for Book
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving (Show)

myBook :: Book
myBook = Book { title = "1984", author = "George Orwell", year = 1949 }

-- =========================
-- MAIN: Test all tasks
main :: IO ()
main = do
  -- HC8T1
  putStrLn "\nHC8T1: Generate Transaction"
  putStrLn (generateTx "Alice" "Bob" 100)

  -- HC8T2
  putStrLn "\nHC8T2: Person Bob"
  print bob

  -- HC8T3
  let c = Circle 5
  let r = Rectangle 10 5
  putStrLn "\nHC8T3: Area of Circle with radius 5"
  print (area c)
  putStrLn "HC8T3: Area of Rectangle with sides 10 and 5"
  print (area r)

  -- HC8T4
  putStrLn "\nHC8T4: Employee Richard"
  print richard

  -- HC8T5
  putStrLn "\nHC8T5: Person1 (Employed)"
  print person1
  putStrLn "HC8T5: Person2 (Unemployed)"
  print person2

  -- HC8T6
  putStrLn "\nHC8T6: Circle instance"
  print myCircle
  putStrLn "HC8T6: Rectangle instance"
  print myRectangle

  -- HC8T7
  putStrLn "\nHC8T7: Describe Dog and Cat"
  putStrLn (describeAnimal dog1)
  putStrLn (describeAnimal cat1)

  -- HC8T8
  putStrLn "\nHC8T8: Greetings"
  putStrLn (greet "Alice" 25)
  putStrLn (greet "Bob" 30)

  -- HC8T9
  putStrLn "\nHC8T9: Create Transaction"
  putStrLn ("Transaction ID: " ++ createTransaction "Alice" "Bob" 100)

  -- HC8T10
  putStrLn "\nHC8T10: Book instance"
  print myBook
