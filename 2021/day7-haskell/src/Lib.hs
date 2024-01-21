module Lib
  ( fn1
  ) where

import Data.List (isPrefixOf)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Inputs
import Lists
import Strings

fn1 :: String -> Int
fn1 line = foldl (\acc a -> acc + abs (median - a)) 0 positions
  where
    positions = sort $ getInts (newDelimiter line ",")
    median =
      if length positions `mod` 2 == 0
        then positions !! ((length positions) `div` 2 - 1)
        else positions !! ((length positions) `div` 2)

--fn2 :: String -> Int
--fn2 line = minimum [totalDistance positions i | i <- [minValue .. maxValue]]
--  where
--    positions = sort $ getInts (newDelimiter line ",")
--    minValue = minimum positions
--    maxValue = maximum positions
--    totalDistance positions target = foldl (\acc a -> acc + 0) 0 positions
--
--totalDistance :: Map.Map Int Int -> [Int] -> Int -> (Map.Map Int Int, Int)
--totalDistance m positions target = foldl updateDistance (m, 0) positions
--  where
--    updateDistance (m, sum) position =
--      let (updatedMap, v) = distance m (abs (position - target))
--      in (updatedMap, sum + v)
--
--distance :: Map.Map Int Int -> Int -> (Map.Map Int Int, Int)
--distance m 0 = (m, 0)
--distance m d
--  | Map.member d m = (m, fromJust $ Map.lookup d m)
--  | otherwise = (Map.insert d (d+v) m, d+ v)
--  where (_, v) = distance m (d-1)

