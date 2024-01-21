import Data.List (isPrefixOf)
import Debug.Trace
import Inputs
import Strings
import Lists
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
fn1 (x:xs) = fn1 xs
