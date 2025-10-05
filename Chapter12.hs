-- Chapter12All.hs
import Data.List (sort)

main :: IO ()
main = do
    putStrLn "Choose a task (1-10):"
    putStrLn "1) Welcome Message"
    putStrLn "2) Add Two Numbers"
    putStrLn "3) Factorial Function"
    putStrLn "4) First 10 Fibonacci Numbers"
    putStrLn "5) Palindrome Checker"
    putStrLn "6) Sort a List of Integers"
    putStrLn "7) Calculate Circle Area"
    putStrLn "8) Merge Two Sorted Lists"
    putStrLn "9) Read and Print File Content"
    putStrLn "10) Mathematical Operations"
    choice <- getLine
    case choice of
        "1" -> task1
        "2" -> task2
        "3" -> task3
        "4" -> task4
        "5" -> task5
        "6" -> task6
        "7" -> task7
        "8" -> task8
        "9" -> task9
        "10" -> task10
        _ -> putStrLn "Invalid choice"

-- Task 1
task1 :: IO ()
task1 = putStrLn "Welcome to Haskell Programming!"

-- Task 2
task2 :: IO ()
task2 = do
    let a = 5
        b = 7
        addTwoNumbers x y = x + y
    putStrLn ("Sum: " ++ show (addTwoNumbers a b))

-- Task 3
task3 :: IO ()
task3 = do
    let n = 5
        factorial 0 = 1
        factorial k = k * factorial (k - 1)
    putStrLn ("Factorial of " ++ show n ++ " is " ++ show (factorial n))

-- Task 4
task4 :: IO ()
task4 = do
    let fib 0 = 0
        fib 1 = 1
        fib n = fib (n - 1) + fib (n - 2)
        first10 = map fib [0..9]
    putStrLn ("First 10 Fibonacci numbers: " ++ show first10)

-- Task 5
task5 :: IO ()
task5 = do
    let testString = "radar"
        isPalindrome s = s == reverse s
    putStrLn ("Is \"" ++ testString ++ "\" a palindrome? " ++ show (isPalindrome testString))

-- Task 6
task6 :: IO ()
task6 = do
    let numbers = [7, 2, 5, 3, 9]
    putStrLn ("Sorted list: " ++ show (sort numbers))

-- Task 7
task7 :: IO ()
task7 = do
    let radius = 4.0
        calculateCircleArea r = pi * r * r
    putStrLn ("Area of circle with radius " ++ show radius ++ " is " ++ show (calculateCircleArea radius))

-- Task 8
task8 :: IO ()
task8 = do
    let list1 = [1,3,5]
        list2 = [2,4,6]
        mergeLists [] ys = ys
        mergeLists xs [] = xs
        mergeLists (x:xs) (y:ys)
            | x <= y    = x : mergeLists xs (y:ys)
            | otherwise = y : mergeLists (x:xs) ys
    putStrLn ("Merged list: " ++ show (mergeLists list1 list2))

-- Task 9
task9 :: IO ()
task9 = do
    let fileContent = "This is the content of the file."
    putStrLn "File content:"
    putStrLn fileContent

-- Task 10
task10 :: IO ()
task10 = do
    let add x y = x + y
        multiply x y = x * y
        a = 3
        b = 4
    putStrLn ("Addition: " ++ show (add a b))
    putStrLn ("Multiplication: " ++ show (multiply a b))
