import Data.List (isPrefixOf)
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

fn1 :: [String] -> Int
fn1 [] = 0
fn1 (x:xs) = parseLine x + fn1 xs

parseLine line = res
  where
    numbersLine = getString (newDelimiter line ": ") 1
    del = newDelimiter numbersLine " | "
    winningStr = getString del 0
    numberStr = getString del 1
    winning = map (\x -> read x :: Int) (filter (/= "") (getStrings (newDelimiter winningStr " ")))
    set = foldl (\acc a -> Set.insert a acc) (Set.empty) winning
    numbers = map (\x -> read x :: Int) (filter (/= "") (getStrings (newDelimiter numberStr " ")))
    res =
      foldl
        (\acc a ->
           if Set.member a set
             then (if acc == 0
                     then 1
                     else acc * 2)
             else acc)
        0
        numbers
