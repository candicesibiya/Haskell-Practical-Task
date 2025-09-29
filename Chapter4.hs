-- HC4T1 - Task 1: Weather Report
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - Task 2: Day Type
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

-- HC4T3 - Task 3: Grade Comment
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n <= 89  = "Good job!"
  | n >= 50 && n <= 69  = "You passed."
  | n >= 0  && n <= 49  = "Better luck next time."
  | otherwise            = "Invalid grade"

-- HC4T4 & HC4T5 - Task 4 & 5: Special Birthday with catch-all
specialBirthday :: Int -> String
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 16 = "Sweet 16!"
specialBirthday 18 = "Welcome to adulthood!"
specialBirthday 21 = "Cheers to 21!"
specialBirthday age = "Happy " ++ show age ++ "th Birthday!"

-- HC4T6 - Task 6: Identify list contents
whatsInsideThisList :: [a] -> String
whatsInsideThisList []        = "The list is empty."
whatsInsideThisList [_]       = "The list has one element."
whatsInsideThisList [_, _]    = "The list has two elements."
whatsInsideThisList [_, _, _] = "The list has three elements."
whatsInsideThisList _         = "The list has more than three elements."

-- HC4T7 - Task 7: First and third elements
firstAndThird :: [a] -> (Maybe a, Maybe a)
firstAndThird []          = (Nothing, Nothing)
firstAndThird [x]         = (Just x, Nothing)
firstAndThird [x, _]      = (Just x, Nothing)
firstAndThird (x:_:z:_)   = (Just x, Just z)

-- HC4T8 - Task 8: Describe Tuple
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, isStudent) =
  name ++ " is " ++ show age ++ " years old and " ++
  (if isStudent then "is a student." else "is not a student.")


-- MAIN: Test all functions
main :: IO ()
main = do
  -- HC4T1
  print (weatherReport "sunny")
  print (weatherReport "rainy")
  print (weatherReport "cloudy")
  print (weatherReport "stormy")

  -- HC4T2
  print (dayType "Monday")
  print (dayType "Saturday")
  print (dayType "Sunday")
  print (dayType "Funday")

  -- HC4T3
  print (gradeComment 95)
  print (gradeComment 75)
  print (gradeComment 60)
  print (gradeComment 40)
  print (gradeComment 110)

  -- HC4T4 & HC4T5
  print (specialBirthday 1)
  print (specialBirthday 16)
  print (specialBirthday 18)
  print (specialBirthday 21)
  print (specialBirthday 30)

  -- HC4T6
  print (whatsInsideThisList [])
  print (whatsInsideThisList [1])
  print (whatsInsideThisList [1,2])
  print (whatsInsideThisList [1,2,3])
  print (whatsInsideThisList [1,2,3,4])

  -- HC4T7
  print (firstAndThird [1,2,3,4,5])
  print (firstAndThird ["a","b","c"])
  print (firstAndThird [10])
  print (firstAndThird [])

  -- HC4T8
  print (describeTuple ("Alice", 20, True))
  print (describeTuple ("Bob", 30, False))
