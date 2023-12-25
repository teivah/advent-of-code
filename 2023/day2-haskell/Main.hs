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

count line = (isPossible gamesStr, gameID)
  where
    (gameID, gamesStr) = parseGame line

parseGame :: String -> (Int, [(Int, String)])
parseGame line = (gameID, games)
  where
    split = splitOn ": " line
    gameID = read (fromJust (stripPrefix "Game " (split !! 0))) :: Int
    gamesStr = concat [splitOn ", " x | x <- splitOn "; " (split !! 1)]
    games = [parsePlay x | x <- gamesStr]

parsePlay :: String -> (Int, String)
parsePlay x = (count, color)
  where
    split = splitOn " " x
    count = read (split !! 0) :: Int
    color = split !! 1

isPossible :: [(Int, String)] -> Bool
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
    (count, color) = x

fn2 :: [String] -> Int
fn2 [] = 0
fn2 (x:xs) = count2 x + fn2 xs

count2 line =
  let (red, green, blue) = fewest gamesStr 0 0 0
   in red * green * blue
  where
    (_, gamesStr) = parseGame line

fewest :: [(Int, String)] -> Int -> Int -> Int -> (Int, Int, Int)
fewest [] red green blue = (red, green, blue)
fewest (x:xs) red green blue
  | color == "red" = fewest xs (max red count) green blue
  | color == "green" = fewest xs red (max green count) blue
  | color == "blue" = fewest xs red green (max blue count)
  where
    (count, color) = x
