{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

-- =====================================================
-- HC21T1: Writer Logging Calculator
-- =====================================================
newtype WriterW a = WriterW { runWriterW :: Writer [String] a }
  deriving (Functor, Applicative, Monad)

addW :: Int -> Int -> WriterW Int
addW x y = WriterW $ writer (x + y, ["add " ++ show x ++ " " ++ show y])

subW :: Int -> Int -> WriterW Int
subW x y = WriterW $ writer (x - y, ["sub " ++ show x ++ " " ++ show y])

mulW :: Int -> Int -> WriterW Int
mulW x y = WriterW $ writer (x * y, ["mul " ++ show x ++ " " ++ show y])

calcDemo :: WriterW Int
calcDemo = do
  a <- addW 1 2
  b <- mulW a 3
  subW b 2

-- =====================================================
-- HC21T2: Writer Functor/Applicative/Monad instances
-- Already derived above

-- =====================================================
-- HC21T3: Writer listen / pass
-- =====================================================
listenW :: WriterW a -> WriterW (a, [String])
listenW (WriterW w) = WriterW $ listen w

passW :: WriterW (a, [String] -> [String]) -> WriterW a
passW (WriterW w) = WriterW $ pass w

-- =====================================================
-- HC21T4: Writer with different monoid (Sum Int)
-- =====================================================
type WriterCount a = Writer (Sum Int) a

demoCount :: WriterCount Int
demoCount = do
  tell (Sum 1)
  tell (Sum 2)
  return 42

-- =====================================================
-- HC21T5: Reader Configurable Greeting
-- =====================================================
data Config = Config { greetPrefix :: String, shout :: Bool }

newtype ReaderC a = ReaderC { runReaderC :: Reader Config a }
  deriving (Functor, Applicative, Monad)

greet :: String -> ReaderC String
greet name = ReaderC $ do
  cfg <- ask
  let base = greetPrefix cfg ++ name
  return $ if shout cfg then map toUpper base else base

-- =====================================================
-- HC21T6: Reader Functor/Applicative/Monad
-- Already derived above

-- =====================================================
-- HC21T7: Reader Mini Env Refactor
-- =====================================================
type EnvApp = Reader (Map String String)

readEnv :: String -> EnvApp (Maybe String)
readEnv key = do
  env <- ask
  return $ Map.lookup key env

-- =====================================================
-- HC21T8: State Primitives get/put/modify
-- =====================================================
newtype StateS s a = StateS { runStateS :: State s a }
  deriving (Functor, Applicative, Monad)

getS :: StateS s s
getS = StateS get

putS :: s -> StateS s ()
putS s = StateS $ put s

modifyS :: (s -> s) -> StateS s ()
modifyS f = StateS $ modify f

-- =====================================================
-- HC21T9: State Counter While Mapping
-- =====================================================
mapCount :: (a -> b) -> [a] -> State Int [b]
mapCount f xs = mapM (\x -> modify (+1) >> return (f x)) xs

-- =====================================================
-- HC21T10: State Vending Machine Core
-- =====================================================
data VendingState = MkVendingState { items :: Int, credit :: Int } deriving Show

insertCoin :: Int -> State VendingState ()
insertCoin amt = modify $ \s -> s { credit = credit s + amt }

vend :: State VendingState String
vend = do
  s <- get
  if items s > 0 && credit s > 0
    then do
      put s { items = items s - 1, credit = credit s - 1 }
      return "Item dispensed"
    else return "Cannot vend"

getChange :: State VendingState Int
getChange = do
  s <- get
  put s { credit = 0 }
  return (credit s)

vendingSequence :: State VendingState (String, Int)
vendingSequence = do
  insertCoin 2
  r <- vend
  c <- getChange
  return (r, c)

-- =====================================================
-- HC21T11: State Undo Stack
-- =====================================================
setValue :: Int -> State (Int,[Int]) ()
setValue newVal = do
  (curr,hist) <- get
  put (newVal, curr:hist)

undo :: State (Int,[Int]) ()
undo = do
  (curr,hist) <- get
  case hist of
    (x:xs) -> put (x,xs)
    []     -> return ()

-- =====================================================
-- HC21T12: State Deterministic Random Walk
-- =====================================================
randomStep :: State (Int,(Int,Int)) ()
randomStep = do
  (seed,(x,y)) <- get
  let seed' = seed * 1103515245 + 12345
      dir = seed' `mod` 4
      (x',y') = case dir of
                  0 -> (x,y+1)
                  1 -> (x,y-1)
                  2 -> (x-1,y)
                  3 -> (x+1,y)
                  _ -> (x,y)
  put (seed',(x',y'))

randomWalk :: Int -> State (Int,(Int,Int)) [(Int,Int)]
randomWalk 0 = do
  (_,pos) <- get
  return [pos]
randomWalk n = do
  (_,pos) <- get
  randomStep
  rest <- randomWalk (n-1)
  return (pos:rest)

-- =====================================================
-- HC21T13: Reader + Writer Configurable Logging
-- =====================================================
stepLog :: String -> Reader Config (Writer [String] ())
stepLog msg = Reader $ \cfg -> writer ((), [greetPrefix cfg ++ msg])

-- =====================================================
-- HC21T14: State + Writer Instrumented State
-- =====================================================
inc :: State Int (Writer [String] ())
inc = do
  modify (+1)
  return $ tell ["Incremented"]

dec :: State Int (Writer [String] ())
dec = do
  modify (\x -> x - 1)
  return $ tell ["Decremented"]

-- =====================================================
-- HC21T15: Reader + State Environment-Driven State Machine
-- =====================================================
tick :: Reader Config (State Int Bool)
tick = Reader $ \cfg -> State $ \s ->
  let s' = s + 1
  in (s' >= 3, s')

-- =====================================================
-- HC21T16: Laws—Associativity & local/ask Invariants
-- =====================================================
-- Example equational tests (can be printed in main or GHCi)
writerAssocTest :: Bool
writerAssocTest = let m = WriterW $ writer (1, ["a"])
                      k x = WriterW $ writer (x+1, ["b"])
                      h x = WriterW $ writer (x*2, ["c"])
                      lhs = do {y <- m; z <- k y; h z}
                      rhs = do {y <- m; z <- (\x -> k x >>= h) y}
                  in runWriterW lhs == runWriterW rhs

-- =====================================================
-- HC21T17: Refactor Legacy Env/Log/State Code
-- =====================================================
type App17 a = ReaderT Env (StateT Int (Writer [String])) a

refactored17 :: App17 Int
refactored17 = do
  env <- ask
  lift $ lift $ tell ["Starting refactored computation"]
  s <- lift get
  lift $ put (s + 1)
  let newVal = s + Map.size env
  lift $ lift $ tell ["Computed value: " ++ show newVal]
  return newVal

-- =====================================================
-- Main Demo
-- =====================================================
main :: IO ()
main = do
  putStrLn "--- HC21T1 calcDemo ---"
  print $ runWriterW calcDemo

  putStrLn "--- HC21T4 demoCount ---"
  print $ runWriter demoCount

  putStrLn "--- HC21T5 greet ---"
  let cfg = Config "Hello, " False
  print $ runReader (runReaderC (greet "Alice")) cfg

  putStrLn "--- HC21T7 readEnv ---"
  print $ runReader (readEnv "DB_PORT") (Map.fromList [("DB_PORT","5432"), ("USER","admin")])

  putStrLn "--- HC21T9 mapCount ---"
  print $ runState (mapCount (+1) [1,2,3]) 0

  putStrLn "--- HC21T10 vendingSequence ---"
  print $ runState vendingSequence (MkVendingState 5 0)

  putStrLn "--- HC21T11 Undo Stack ---"
  print $ runState (setValue 10 >> setValue 20 >> undo) (0,[])

  putStrLn "--- HC21T12 Random Walk ---"
  print $ runState (randomWalk 5) (123,(0,0))

  putStrLn "--- HC21T17 Refactored Legacy ---"
  let env = Map.fromList [("DB_PORT","5432"),("USER","admin")]
      initialState = 0
      ((val, finalState), logs) = runWriter $ runStateT (runReaderT refactored17 env) initialState
  putStrLn $ "Result value: " ++ show val
  putStrLn "Logs:"
  mapM_ putStrLn logs
  putStrLn $ "Final state: " ++ show finalState
