-- Haskell Chapter 11: Simple I/O Programs

import Data.Char (toUpper)

-- HC11T1: Greet the User
greetUser :: IO ()
greetUser = do
  putStrLn "HC11T1: What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"

-- HC11T2: Count Characters in a Line
countChars :: IO ()
countChars = do
  putStrLn "HC11T2: Enter a line:"
  line <- getLine
  putStrLn $ "Number of characters: " ++ show (length line)

-- HC11T3: Double a Number
doubleNumber :: IO ()
doubleNumber = do
  putStrLn "HC11T3: Enter a number:"
  input <- getLine
  let num = read input :: Int
  putStrLn $ "Double: " ++ show (num * 2)

-- HC11T4: Concatenate Two Lines
concatTwoLines :: IO ()
concatTwoLines = do
  putStrLn "HC11T4: Enter first line:"
  line1 <- getLine
  putStrLn "Enter second line:"
  line2 <- getLine
  putStrLn $ "Concatenated: " ++ line1 ++ line2

-- HC11T5: Repeat Until "quit"
repeatUntilQuit :: IO ()
repeatUntilQuit = do
  putStrLn "HC11T5: Type something (type 'quit' to exit):"
  loop
  where
    loop = do
      input <- getLine
      if input == "quit"
        then putStrLn "Goodbye!"
        else do
          putStrLn $ "You typed: " ++ input
          loop

-- HC11T6: Uppercase Converter
uppercaseConverter :: IO ()
uppercaseConverter = do
  putStrLn "HC11T6: Enter a line:"
  line <- getLine
  putStrLn $ "Uppercase: " ++ map toUpper line

-- HC11T7: User Options
userOptions :: IO ()
userOptions = do
  putStrLn "HC11T7: Choose an option: 1) Greet 2) Double 3) Quit"
  option <- getLine
  case option of
    "1" -> putStrLn "You chose Greet!"
    "2" -> putStrLn "You chose Double!"
    "3" -> putStrLn "Exiting..."
    _   -> putStrLn "Invalid option"

-- HC11T8: Even or Odd Checker
evenOddChecker :: IO ()
evenOddChecker = do
  putStrLn "HC11T8: Enter a number:"
  input <- getLine
  let num = read input :: Int
  if even num
    then putStrLn $ show num ++ " is even."
    else putStrLn $ show num ++ " is odd."

-- HC11T9: Sum Two Numbers
sumTwoNumbers :: IO ()
sumTwoNumbers = do
  putStrLn "HC11T9: Enter first number:"
  n1 <- fmap read getLine :: IO Int
  putStrLn "Enter second number:"
  n2 <- fmap read getLine :: IO Int
  putStrLn $ "Sum: " ++ show (n1 + n2)

-- HC11T10: Reverse User Input
reverseInput :: IO ()
reverseInput = do
  putStrLn "HC11T10: Enter a line to reverse:"
  line <- getLine
  putStrLn $ "Reversed: " ++ reverse line

-- MAIN FUNCTION TO RUN ALL TASKS
main :: IO ()
main = do
  putStrLn "=== Haskell Chapter 11 Practical Tasks ==="
  
  
  putStrLn "\nAll tasks loaded. Uncomment a function in main to test it."
