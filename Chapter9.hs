-- Haskell Chapter 9 Practical Tasks: Parametric Types and Recursion

-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)
-- Example: Entity Int could be ("Address", 42)

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving (Show)

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n Empty = Empty
addN n (Has x) = Has (n + x)

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a
  = Circle { color :: a, radius :: Float }
  | Rectangle { color :: a, width :: Float, height :: Float }
  deriving (Show)

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving (Show)

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence Data Type
data Sequence a = Nil | Node a (Sequence a) deriving (Show)

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ Nil = False
elemSeq x (Node y ys)
  | x == y    = True
  | otherwise = elemSeq x ys

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyBST | NodeBST a (BST a) (BST a) deriving (Show)


-- MAIN FUNCTION TO DEMONSTRATE OUTPUTS
main :: IO ()
main = do
  putStrLn "=== Chapter 9 Practical Tasks Demonstration ==="

  -- HC9T1 Example
  let student :: Entity String
      student = ("Campus Address", "123 Smart St")
  print student

  -- HC9T2 & HC9T3 Example
  let box1 = Has 10
  let box2 = addN 5 box1
  print box1
  print box2

  -- HC9T4 Example
  print (extract 0 Empty)
  print (extract 0 (Has 42))

  -- HC9T5 Example
  let c1 = Circle "Red" 4.5
  let r1 = Rectangle "Blue" 3.0 6.0
  print c1
  print r1

  -- HC9T6 & HC9T7 Example
  let reply1 = Tweet "Nice!" 5 []
  let reply2 = Tweet "Great post!" 3 []
  let mainTweet = Tweet "Hello World!" 10 [reply1, reply2]
  print (engagement mainTweet)

  -- HC9T8 & HC9T9 Example
  let seq1 = Node 1 (Node 2 (Node 3 Nil))
  print seq1
  print (elemSeq 2 seq1)
  print (elemSeq 5 seq1)

  -- HC9T10 Example
  let bst = NodeBST 5 (NodeBST 3 EmptyBST EmptyBST) (NodeBST 7 EmptyBST EmptyBST)
  print bst
