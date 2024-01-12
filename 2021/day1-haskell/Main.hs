import Data.List (isPrefixOf)
import Debug.Trace
import Lib.Inputs
import Lib.Lists
import Lib.Strings
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res
  let res = fn2 linesList
  print res

fn1 :: [String] -> Int
fn1 lines = sum [1 | (a, b) <- zip numbers (tail numbers), b > a]
  where
    numbers = [read x :: Int | x <- lines]

fn2 :: [String] -> Int
fn2 lines = count (numbers !! 0 + numbers !! 1 + numbers !! 2) (drop 1 numbers)
  where
    numbers = [read x :: Int | x <- lines]

count :: Int -> [Int] -> Int
count previous (a:b:[]) = 0
count previous (a:b:c:d) = x + count v (b : c : d)
  where
    v = a + b + c
    x =
      if v > previous
        then 1
        else 0
