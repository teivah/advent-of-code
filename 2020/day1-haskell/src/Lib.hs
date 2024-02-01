module Lib
  ( fn1
  , fn2
  ) where

import Data.List (sort)

target = 2020

toSorted :: [String] -> [Int]
toSorted lines = sort $ map (\s -> read s :: Int) lines

fn1 :: [String] -> Int
fn1 lines = find1 sorted (0, length (sorted) - 1)
  where
    sorted = toSorted lines

find1 :: [Int] -> (Int, Int) -> Int
find1 xs (l, r)
  | sum == target = a * b
  | sum < target = find1 xs (l + 1, r)
  | otherwise = find1 xs (l, r - 1)
  where
    a = xs !! l
    b = xs !! r
    sum = a + b

fn2 :: [String] -> Int
fn2 lines = head $ filter (/= -1) $ map res [i | i <- [0 .. length (sorted) - 2]]
  where
    sorted = toSorted lines
    res i = find2 sorted i (i + 1, length (sorted) - 1)

find2 :: [Int] -> Int -> (Int, Int) -> Int
find2 [] _ _ = -1
find2 xs i (l, r)
  | l >= r = -1
  | sum == target = a * b * x
  | sum < target = find2 xs i (l + 1, r)
  | otherwise = find2 xs i (l, r - 1)
  where
    x = xs !! i
    a = xs !! l
    b = xs !! r
    sum = a + b + x
