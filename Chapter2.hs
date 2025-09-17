Data types, Signatures, and Polymorphism

Hakell Practical Tasks
Write code to complete Practical Tasks below. Haskell editor for testing is also found below this page.

HC2T1 - Task 1: Checking Types in GHCi
Open GHCi and check the types of the following expressions:

42
3.14
"Haskell"
'Z'
True && False
Write down the expected types before checking in GHCi.

HC2T2 - Task 2: Function Type Signatures
Write function signatures for the following functions:

A function add that takes two Int values and returns their sum.
A function isEven that takes an Int and returns a Bool indicating if it's even.
A function concatStrings that takes two String values and returns their concatenation.
Implement these functions.

HC2T3 - Task 3: Immutable Variables
Define the following immutable variables in Haskell:

myAge as an Int
piValue as a Double
greeting as a String
isHaskellFun as a Bool
Try modifying one of the variables and observe what happens.

HC2T4 - Task 4: Converting Between Infix and Prefix Notations
Use prefix notation for the following infix expressions:

5 + 3
10 * 4
True && False
Use infix notation for the following prefix functions:

(+) 7 2
(*) 6 5
(&&) True False
HC2T5 - Task 5: Defining and Using Functions
Write a function circleArea that takes a Float radius and returns the area of the circle.
Write a function maxOfThree that takes three Int values and returns the maximum.
Test your functions with different inputs.
HC2T6 - Task 6: Understanding Int vs Integer
Define an Int variable smallNumber with the value 262.
Define an Integer variable bigNumber with the value 2127.
Try to evaluate 2^64 :: Int in GHCi and note the result.
HC2T7 - Task 7: Boolean Expressions
Write Boolean expressions that evaluate to:

True using &&
False using ||
True using not
A comparison that returns False
