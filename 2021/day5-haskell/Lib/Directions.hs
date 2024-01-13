module Lib.Directions
  ( Direction(..)
  , Position(..)
  , newPosition
  , move
  , delta
  , rev
  , turn
  ) where

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  | DirUpLeft
  | DirUpRight
  | DirDownLeft
  | DirDownRight
  deriving (Show, Eq, Ord)

rev :: Direction -> Direction
rev DirUp = DirDown
rev DirDown = DirUp
rev DirLeft = DirRight
rev DirRight = DirLeft
rev DirUpLeft = DirDownRight
rev DirUpRight = DirDownLeft
rev DirDownLeft = DirUpRight
rev DirDownRight = DirUpLeft

turn :: Direction -> Direction -> Direction
turn DirUp turn = turn
turn DirDown turn = rev turn
turn DirLeft DirLeft = DirDown
turn DirLeft DirRight = DirUp
turn DirRight DirLeft = DirUp
turn DirRight DirRight = DirDown

data Position = Position
  { row :: Int
  , col :: Int
  } deriving (Show, Eq, Ord)

newPosition :: Int -> Int -> Position
newPosition row col = Position {row = row, col = col}

move :: Position -> Direction -> Int -> Position
move p DirUp moves = delta p (-moves) 0
move p DirDown moves = delta p (moves) 0
move p DirLeft moves = delta p 0 (-moves)
move p DirRight moves = delta p 0 (moves)
move p DirUpLeft moves = delta p (-moves) (-moves)
move p DirUpRight moves = delta p (-moves) (moves)
move p DirDownLeft moves = delta p (moves) (-moves)
move p DirDownRight moves = delta p (moves) (moves)

delta :: Position -> Int -> Int -> Position
delta p r c = newPosition ((row p) + r) ((col p) + c)
