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
  let res = fn2 (reverse linesList) []
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

fn2 :: [String] -> [Int] -> Int
fn2 [] cache = sum cache
fn2 (x:xs) cache = fn2 xs cache2
  where
    v = parseLine2 x cache
    cache2 = v : cache

parseLine2 :: String -> [Int] -> Int
parseLine2 line cache = res
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
    won = foldr (\a acc -> (a + id) : acc) [] [1 .. matching]
    res = 1 + (foldl (\acc a -> acc + (cache !! (a - id - 1))) 0 won)
