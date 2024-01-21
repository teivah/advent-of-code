import Control.Monad (replicateM)
import Control.Monad.State
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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
  let res = fn1 80 $ linesList !! 0
  print res
  let res = fn2 256 $ linesList !! 0
  print res

fn1 :: Int -> String -> Int
fn1 days line = length $ simulate days fishes
  where
    fishes = getInts $ newDelimiter line ","

simulate :: Int -> [Int] -> [Int]
-- execState returns the final state
-- replicateM replicates a monadic action a specified number of times
simulate days fishes = execState (replicateM days simulateOneDay) fishes

simulateOneDay :: State [Int] ()
simulateOneDay =
  state $ \fishes -- fishes represents the current state
   ->
    let (existing, additional) = foldr simulateFish ([], []) fishes
     in ((), existing ++ additional)

simulateFish :: Int -> ([Int], [Int]) -> ([Int], [Int])
simulateFish len (existing, additional)
  | len <= 0 = (8 : existing, 6 : additional)
  | otherwise = (existing, (len - 1) : additional)

fn2 :: Int -> String -> Int
fn2 days line = foldl (\acc (_, v) -> acc + v) 0 (Map.toList m2)
  where
    fishes = getInts $ newDelimiter line ","
    m = foldl (\acc a -> Map.insertWith (+) a 1 acc) Map.empty fishes
    m2 = solve days m

solve :: Int -> Map.Map Int Int -> Map.Map Int Int
solve 0 m = m
solve days m = solve (days - 1) res
  where
    res = foldl updateMap (Map.empty) (Map.toList m)
    updateMap acc (age, count)
      | age == 0 = Map.insertWith (+) 6 count $ Map.insertWith (+) 8 count acc
      | otherwise = Map.insertWith (+) (age - 1) count acc
