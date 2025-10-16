-- Chapter 13 Practical Tasks: Working with Modules and Directories


module Main where

import System.Directory
import Data.List
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Char as C

import SumNonEmpty (sumNonEmpty)

------------------------------------------------------------
-- HC13T1: List Files in Directory
------------------------------------------------------------
listFiles :: IO [FilePath]
listFiles = do
    files <- listDirectory "."
    return files

------------------------------------------------------------
-- HC13T2: Filter Files by Substring
------------------------------------------------------------
filterFiles :: String -> IO [FilePath]
filterFiles sub = do
    files <- listFiles
    return (filter (isInfixOf sub) files)

------------------------------------------------------------
-- HC13T3: Sort and Return Filtered Files
------------------------------------------------------------
sortedFilteredFiles :: String -> IO [FilePath]
sortedFilteredFiles sub = do
    filtered <- filterFiles sub
    return (sort filtered)

------------------------------------------------------------
-- HC13T6: File Names to Map
------------------------------------------------------------
filesToMap :: [FilePath] -> Map.Map Int FilePath
filesToMap files = Map.fromList (zip [1..] files)

------------------------------------------------------------
-- HC13T8: Qualified Imports Example
------------------------------------------------------------
qualifiedExample :: String -> String
qualifiedExample str =
    "Uppercase: " ++ (L.intercalate ", " (map (:[]) (map C.toUpper str)))

------------------------------------------------------------
-- HC13T9: Renaming Module Namespace
------------------------------------------------------------
renamingExample :: String -> String
renamingExample str =
    "Sorted letters: " ++ show (L.sort str)

------------------------------------------------------------
-- HC13T10: Main Function Calling All Tasks
------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== HC13T1: List Files in Directory ==="
    files <- listFiles
    print files

    putStrLn "\n=== HC13T2: Filter Files by 'hs' ==="
    filtered <- filterFiles "hs"
    print filtered

    putStrLn "\n=== HC13T3: Sorted Filtered Files ==="
    sorted <- sortedFilteredFiles "hs"
    print sorted

    putStrLn "\n=== HC13T6: Convert to Map ==="
    print (filesToMap sorted)

    putStrLn "\n=== HC13T7: Use Custom Module (SumNonEmpty) ==="
    let nums = [3, 6, 9]
    print (sumNonEmpty nums)

    putStrLn "\n=== HC13T8: Qualified Imports Example ==="
    putStrLn (qualifiedExample "hello")

    putStrLn "\n=== HC13T9: Renaming Module Namespace ==="
    putStrLn (renamingExample "haskell")

    putStrLn "\n=== HC13T10: Multi-Module Main Completed Successfully ==="
