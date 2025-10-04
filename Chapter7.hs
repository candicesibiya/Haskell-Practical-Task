-- HC7T1: Implement an Eq Instance for a Custom Data Type
data Color = Red | Green | Blue
  deriving (Show, Read, Enum, Bounded)

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False


-- HC7T2: Implement an Ord Instance for a Custom Data Type
instance Ord Color where
  compare Red Red = EQ
  compare Red _ = LT
  compare Green Red = GT
  compare Green Green = EQ
  compare Green Blue = LT
  compare Blue Blue = EQ
  compare Blue _ = GT


-- HC7T3: Function Using Multiple Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y


-- HC7T4: Custom Type with Show and Read
data Shape = Circle Double | Rectangle Double Double
  deriving (Eq, Ord)

instance Show Shape where
  show (Circle r) = "Circle with radius " ++ show r
  show (Rectangle w h) = "Rectangle with width " ++ show w ++ " and height " ++ show h

instance Read Shape where
  readsPrec _ value =
    case words value of
      ["Circle", r] -> [(Circle (read r), "")]
      ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
      _ -> []


-- HC7T5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea side = side * side


-- HC7T6: Using Integral and Floating Type Classes
circleCircumference :: (Floating a) => a -> a
circleCircumference r = 2 * pi * r


-- HC7T7: Bounded and Enum
nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise = succ c


-- HC7T8: Parse a Value from a String Using Read
parseShape :: String -> Maybe Shape
parseShape s = case reads s of
  [(shape, "")] -> Just shape
  _ -> Nothing


-- HC7T9: Type Class with Multiple Instances
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True = "This value is True."
  describe False = "This value is False."

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h


-- HC7T10: Function with Multiple Type Class Constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y =
  if x >= y
    then describe x
    else describe y


-- MAIN
main :: IO ()
main = do
  putStrLn "HC7T1 & HC7T2: Eq and Ord for Color"
  print (Red == Green)
  print (Red < Blue)

  putStrLn "\nHC7T3: Compare two values (5 and 8)"
  print (compareValues 5 8)

  putStrLn "\nHC7T4: Show and Read Shape"
  print (show (Circle 5.0))
  print (read "Circle 5.0" :: Shape)

  putStrLn "\nHC7T5: Area of a square with side 4"
  print (squareArea 4)

  putStrLn "\nHC7T6: Circumference of circle radius 7"
  print (circleCircumference 7)

  putStrLn "\nHC7T7: Next color after Blue (wraps around)"
  print (nextColor Blue)

  putStrLn "\nHC7T8: Parse Shape from string"
  print (parseShape "Circle 10.0")

  putStrLn "\nHC7T9: Describe Bool and Shape"
  print (describe True)
  print (describe (Rectangle 3 6))

  putStrLn "\nHC7T10: Describe and Compare Shapes"
  print (describeAndCompare (Circle 3) (Rectangle 3 6))
