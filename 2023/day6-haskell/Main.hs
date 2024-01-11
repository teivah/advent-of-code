import Data.List (isPrefixOf)
import Data.List (foldl')
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
  print $ fn1 linesList
  print $ fn2 linesList

--  print $ foldl (\acc a -> acc + (travelDistance a 10 10)) 0 [1 .. 10]
fn1 :: [String] -> Int
fn1 line = res times distances
  where
    times = parse1 (line !! 0)
    distances = parse1 (line !! 1)

parse1 :: String -> [Int]
parse1 s = [read x :: Int | x <- getStrings del, x /= ""]
  where
    line = trimSpace $ substringAfter s ": "
    del = newDelimiter line " "

res :: [Int] -> [Int] -> Int
res [] [] = 1
res (x:xs) (y:ys) = n * res xs ys
  where
    n = numberOfWays x y

numberOfWays :: Int -> Int -> Int
numberOfWays time distance = foldl' (\acc i -> acc + (travelDistance i time distance)) 0 [1 .. (time - 1)]

travelDistance :: Int -> Int -> Int -> Int
travelDistance hold raceTime distance =
  if (raceTime - hold) * hold > distance
    then 1
    else 0

fn2 :: [String] -> Int
fn2 line = numberOfWays time distance
  where
    time = parse2 (line !! 0)
    distance = parse2 (line !! 1)

parse2 :: String -> Int
parse2 s = read (concat [x | x <- getStrings del, x /= ""]) :: Int
  where
    line = trimSpace $ substringAfter s ": "
    del = newDelimiter line " "
