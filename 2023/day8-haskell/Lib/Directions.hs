module Lib.Directions
  ( Direction(..)
  ) where

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)
