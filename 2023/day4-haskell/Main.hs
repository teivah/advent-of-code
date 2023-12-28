import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Debug.Trace
import Lib.Inputs
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res
  let res = fn2 linesList [] []
  print res

fn1 :: [String] -> Int
fn1 [] = 0
fn1 (x:xs) = parseLine x + fn1 xs

parseLine :: String -> Int
parseLine line = matching
  where
    numbersLine = getString (newDelimiter line ": ") 1
    del = newDelimiter numbersLine " | "
    winningStr = getString del 0
    numberStr = getString del 1
    winning = map (\x -> read x :: Int) (filter (/= "") (getStrings $ newDelimiter winningStr " "))
    set = foldl (\acc a -> Set.insert a acc) (Set.empty) winning
    numbers = map (\x -> read x :: Int) (filter (/= "") (getStrings $ newDelimiter numberStr " "))
    matching =
      foldl
        (\acc a ->
           if Set.member a set
             then (if acc == 0
                     then 1
                     else acc * 2)
             else acc)
        0
        numbers

-- Lines, cache, remaining
fn2 :: [String] -> [[Int]] -> [Int] -> Int
fn2 [] _ [] = 0
fn2 [] cache (x:xs) = 1 + fn2 [] cache (xs ++ v)
  where
    v = cache !! (x - 1)
--1 + fn2 [] cache (xs ++ v)
--        Just _ -> error "stop"
fn2 (x:xs) cache rem = 1 + (fn2 xs (cache ++ [v]) (rem ++ v))
  where
    (id, v) = parseLine' x

parseLine' :: String -> (Int, [Int])
parseLine' line = (id, res)
  where
    numbersLine = getString (newDelimiter line ": ") 1
    del = newDelimiter numbersLine " | "
    id = read (filter (/= ' ') (drop 5 (getString (newDelimiter line ": ") 0))) :: Int
    winningStr = getString del 0
    numberStr = getString del 1
    winning = map (\x -> read x :: Int) (filter (/= "") (getStrings $ newDelimiter winningStr " "))
    set = foldl (\acc a -> Set.insert a acc) (Set.empty) winning
    numbers = map (\x -> read x :: Int) (filter (/= "") (getStrings $ newDelimiter numberStr " "))
    matching =
      foldl
        (\acc a ->
           if Set.member a set
             then acc + 1
             else acc)
        0
        numbers
    res = foldr (\a acc -> (a + id) : acc) [] [1 .. matching]
