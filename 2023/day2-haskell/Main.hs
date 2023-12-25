import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace
import System.IO

main :: IO ()
main = do
  res <- withFile "input.txt" ReadMode (\handle -> fn1 handle)
  print res

maxRed = 12

maxGreen = 13

maxBlue = 14

fn1 :: Handle -> IO Int
fn1 handle = do
  eof <- hIsEOF handle
  if eof
    then return 0
    else do
      line <- hGetLine handle
      let (isPossible, id) = count line
      print isPossible
      if not isPossible
        then fn1 handle
        else do
          next <- fn1 handle
          return (id + next)

count line = (isPossibleList gamesStr2, gameID)
  where
    split = splitOn ": " line
    gameID = read (fromJust (stripPrefix "Game " (split !! 0))) :: Int
    gamesStr = splitOn "; " (split !! 1)
    gamesStr2 = [splitOn ", " x | x <- gamesStr]

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
