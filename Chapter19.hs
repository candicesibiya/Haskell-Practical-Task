-- Haskell Chapter 19 Practical Tasks: Applicative Functors and Effects

{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (forever, replicateM_, when)
import Control.Applicative (liftA2)

-- HC19T1: Applicative Instance for Pair
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

hc19t1 :: IO ()
hc19t1 = print $ Pair (+1) (*2) <*> Pair 3 4
-- Expected: Pair 4 8

-- HC19T2: addThreeApplicative
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative a b c = (\x y z -> x + y + z) <$> a <*> b <*> c

hc19t2 :: IO ()
hc19t2 = do
    print $ addThreeApplicative (Just 1) (Just 2) (Just 3) -- Just 6
    print $ addThreeApplicative (Just 5) Nothing (Just 2)    -- Nothing

-- HC19T3: safeProduct
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = foldr (liftA2 (*)) (Just 1)

hc19t3 :: IO ()
hc19t3 = do
    print $ safeProduct [Just 2, Just 3, Just 4] -- Just 24
    print $ safeProduct [Just 2, Nothing, Just 3] -- Nothing

-- HC19T4: liftAndMultiply with liftA2
liftAndMultiply :: Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2 (*)

hc19t4 :: IO ()
hc19t4 = print $ liftAndMultiply (Just 5) (Just 6) -- Just 30

-- HC19T5: applyEffects with <*>
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (a,b) = (+) <$> a <*> b

hc19t5 :: IO ()
hc19t5 = do
    result <- applyEffects (return 3, return 7)
    print result -- 10

-- HC19T6: repeatEffect with forever
repeatEffect :: IO () -> IO ()
repeatEffect = forever

hc19t6 :: IO ()
hc19t6 = repeatEffect (putStrLn "Repeating forever (Ctrl+C to stop)")

-- HC19T7: conditionalPrint with when
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

hc19t7 :: IO ()
hc19t7 = do
    conditionalPrint True "This prints!"
    conditionalPrint False "This will not print."

-- HC19T8: discardSecond with <*
discardSecond :: IO a -> IO b -> IO a
discardSecond = (<*)

hc19t8 :: IO ()
hc19t8 = do
    let first = putStrLn "First" >> return 10
        second = putStrLn "Second" >> return 20
    result <- discardSecond first second
    print result -- 10

-- HC19T9: pureAndApply Demonstration
pureAndApply :: Maybe Int
pureAndApply = pure (+) <*> Just 5 <*> Just 10

hc19t9 :: IO ()
hc19t9 = print pureAndApply -- Just 15

-- HC19T10: combineResults for Either
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults a b = (+) <$> a <*> b

hc19t10 :: IO ()
hc19t10 = do
    print $ combineResults (Right 5) (Right 10)
    print $ combineResults (Left "Error") (Right 10)

-- HC19T11: Applicative Instance for Wrapper
data Wrapper a = Wrapper a deriving (Show)

instance Functor Wrapper where
    fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
    pure = Wrapper
    (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

hc19t11 :: IO ()
hc19t11 = print $ Wrapper (+3) <*> Wrapper 7 -- Wrapper 10

-- HC19T12: sumThreeApplicative for Either String Int
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative a b c = (\x y z -> x + y + z) <$> a <*> b <*> c

hc19t12 :: IO ()
hc19t12 = do
    print $ sumThreeApplicative (Right 2) (Right 3) (Right 4)
    print $ sumThreeApplicative (Right 5) (Left "Error") (Right 2)

-- HC19T13: whenApplicative Function
whenApplicative :: Bool -> IO () -> IO ()
whenApplicative = when

hc19t13 :: IO ()
hc19t13 = do
    whenApplicative True (putStrLn "Condition True")
    whenApplicative False (putStrLn "Condition False")

-- HC19T14: replicateEffect with replicateM
replicateEffect :: Int -> IO () -> IO ()
replicateEffect = replicateM_

hc19t14 :: IO ()
hc19t14 = replicateEffect 3 (putStrLn "Hello!")

-- HC19T15: sequenceEffects for Applicative List
sequenceEffects :: [IO a] -> IO [a]
sequenceEffects = sequenceA

hc19t15 :: IO ()
hc19t15 = do
    let actions = [putStrLn "A" >> return 1,
                   putStrLn "B" >> return 2,
                   putStrLn "C" >> return 3]
    results <- sequenceEffects actions
    print results

-- HC19T16: applyWithEffects and <*>
applyWithEffects :: IO Int -> IO Int -> IO Int
applyWithEffects ioA ioB = (+) <$> ioA <*> ioB

hc19t16 :: IO ()
hc19t16 = do
    let a = putStrLn "Effect A" >> return 5
        b = putStrLn "Effect B" >> return 10
    result <- applyWithEffects a b
    print result

-- HC19T17: simulateMaybeEffect
simulateMaybeEffect :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
simulateMaybeEffect a b c = (\x y z -> x + y + z) <$> a <*> b <*> c

hc19t17 :: IO ()
hc19t17 = do
    print $ simulateMaybeEffect (Just 2) (Just 3) (Just 4)
    print $ simulateMaybeEffect (Just 5) Nothing (Just 1)

-- HC19T18: combineEitherResults
combineEitherResults :: Either String Int -> Either String Int -> Either String Int -> Either String Int
combineEitherResults a b c = (\x y z -> x + y + z) <$> a <*> b <*> c

hc19t18 :: IO ()
hc19t18 = do
    print $ combineEitherResults (Right 2) (Right 3) (Right 4)
    print $ combineEitherResults (Left "Error A") (Right 2) (Right 3)

-- HC19T19: sequenceApplicative for Maybe List
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

hc19t19 :: IO ()
hc19t19 = do
    print $ sequenceApplicative [Just 1, Just 2, Just 3]
    print $ sequenceApplicative [Just 1, Nothing, Just 3]

-- HC19T20: replicateForever with forever
replicateForever :: IO () -> IO ()
replicateForever = forever

hc19t20 :: IO ()
hc19t20 = replicateForever (putStrLn "Repeating forever (Ctrl+C to stop)")

-- Optional: A main that lets you test any task
main :: IO ()
main = do
    putStrLn "Choose a task to run (1-20). For example, run hc19t1."
    -- Example: run hc19t1
    hc19t1
