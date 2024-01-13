module Lib.Lists
  ( sublist
  ) where

sublist :: Int -> Int -> [a] -> [a]
sublist start end xs = take (end - start) $ drop start xs
