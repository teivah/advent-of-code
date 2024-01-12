import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Debug.Trace
import Lib.Conversions
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

fn1 :: [String] -> Int
fn1 lines = binaryToDecimal gr * binaryToDecimal er
  where
    gr = gammaRate lines
    er =
      [ if x == '0'
        then '1'
        else '0'
      | x <- gr
      ]

gammaRate :: [String] -> String
gammaRate lines = [gammaRateCol lines col 0 0 | col <- [0 .. ((length (lines !! 0)) - 1)]]
  where
    gammaRateCol :: [String] -> Int -> Int -> Int -> Char
    gammaRateCol [] _ zero one =
      if zero > one
        then '0'
        else '1'
    gammaRateCol (x:xs) col zero one =
      if (x !! col) == '0'
        then gammaRateCol xs col (zero + 1) one
        else gammaRateCol xs col zero (one + 1)
