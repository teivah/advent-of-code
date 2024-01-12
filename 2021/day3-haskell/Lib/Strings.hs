module Lib.Strings
  ( substringAfter
  , trimSpace
  ) where

import Data.List (isPrefixOf, tails)
import Data.Char (isSpace)

substringAfter :: String -> String -> String
substringAfter s sub =
  case findIndex (sub `isPrefixOf`) (tails s) of
    Just v -> drop (v + length sub) s
    Nothing -> error "substring not found"

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex predicate = loop 0
  where
    loop _ [] = Nothing
    loop n (x:xs)
      | predicate x = Just n
      | otherwise = loop (n + 1) xs

trimSpace :: String -> String
trimSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse
