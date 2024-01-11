import Data.List (isPrefixOf)
import Data.Text (Text, strip)
import Debug.Trace
import Lib.Inputs
import Lib.Strings
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res

fn1 :: [String] -> Int
fn1 line = res times distances
  where
    times = parse (line !! 0)
    distances = parse (line !! 1)

parse :: String -> [Int]
parse s = res
  where
    line = trimSpace $ substringAfter s ": "
    del = newDelimiter line " "
    res = [read x :: Int | x <- getStrings del, x /= ""]

res :: [Int] -> [Int] -> Int
res [] [] = 1
res (x:xs) (y:ys) = n * res xs ys
  where
    n = numberOfWays 0 x y

numberOfWays :: Int -> Int -> Int -> Int
numberOfWays i time distance
  | i == time = 0
  | otherwise = sum + numberOfWays (i + 1) time distance
  where
    sum =
      if ((time - i) * i) > distance
        then 1
        else 0
