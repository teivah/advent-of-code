import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace
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

maxRed = 12

maxGreen = 13

maxBlue = 14

fn1 :: [String] -> Int
fn1 [] = 0
fn1 (x:xs)
  | isPossible = id + fn1 xs
  | otherwise = fn1 xs
  where
    (isPossible, id) = count x

count line = (isPossibleList gamesStr, gameID)
  where
    (gameID, gamesStr) = parseGame line

parseGame :: String -> (Int, [[String]])
parseGame line = (gameID, gamesStr)
  where
    split = splitOn ": " line
    gameID = read (fromJust (stripPrefix "Game " (split !! 0))) :: Int
    gamesStr = [splitOn ", " x | x <- splitOn "; " (split !! 1)]

isPossibleList :: [[String]] -> Bool
isPossibleList [] = True
isPossibleList (x:xs) =
  if not (isPossible x)
    then False
    else isPossibleList xs

isPossible :: [String] -> Bool
isPossible [] = True
isPossible (x:xs)
  | color == "red" =
    if count > maxRed
      then False
      else isPossible xs
  | color == "green" =
    if count > maxGreen
      then False
      else isPossible xs
  | color == "blue" =
    if count > maxBlue
      then False
      else isPossible xs
  where
    split = splitOn " " x
    count = read (split !! 0) :: Int
    color = split !! 1

fn2 :: [String] -> Int
fn2 [] = 0
fn2 (x:xs) = count2 x + fn2 xs

count2 line =
  let (red, green, blue) = fewestList gamesStr 0 0 0
   in red * green * blue
  where
    (_, gamesStr) = parseGame line

fewestList :: [[String]] -> Int -> Int -> Int -> (Int, Int, Int)
fewestList [] red green blue = (red, green, blue)
fewestList (x:xs) red green blue = do
  let (red2, green2, blue2) = fewest x 0 0 0
  fewestList xs (max red red2) (max green green2) (max blue blue2)

fewest :: [String] -> Int -> Int -> Int -> (Int, Int, Int)
fewest [] red green blue = (red, green, blue)
fewest (x:xs) red green blue
  | color == "red" = fewest xs (max red count) green blue
  | color == "green" = fewest xs red (max green count) blue
  | color == "blue" = fewest xs red green (max blue count)
  where
    split = splitOn " " x
    count = read (split !! 0) :: Int
    color = split !! 1
