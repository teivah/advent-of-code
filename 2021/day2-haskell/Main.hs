import Data.List (isPrefixOf)
import Debug.Trace
import Lib.Directions
import Lib.Inputs
import Lib.Lists
import Lib.Strings
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 (newPosition 0 0) linesList
  print res
  let res = fn2 0 (newPosition 0 0) linesList
  print res

fn1 :: Position -> [String] -> Int
fn1 pos [] = (row pos) * (col pos)
fn1 pos (x:xs) =
  case action of
    "forward" -> fn1 (move pos DirRight n) xs
    "down" -> fn1 (move pos DirDown n) xs
    "up" -> fn1 (move pos DirUp n) xs
  where
    del = newDelimiter x " "
    action = getString del 0
    n = getInt del 1

fn2 :: Int -> Position -> [String] -> Int
fn2 _ pos [] = (row pos) * (col pos)
fn2 aim pos (x:xs) =
  case action of
    "forward" -> fn2 aim (move (move pos DirRight n) DirDown (aim * n)) xs
    "down" -> fn2 (aim + n) pos xs
    "up" -> fn2 (aim - n) pos xs
  where
    del = newDelimiter x " "
    action = getString del 0
    n = getInt del 1
