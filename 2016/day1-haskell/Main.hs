import Data.List.Split (splitOn)
import System.IO

data Direction
  = North
  | South
  | East
  | West

main :: IO ()
main = do
  content <- readFile "input.txt"
  let s = splitOn ", " content
  let res = fn1 s North 0 0
  print res

fn1 :: [String] -> Direction -> Int -> Int -> Int
fn1 [] _ row col = abs row + abs col
fn1 (x:xs) dir row col = do
  let (to, count) = parse x
  let dir2 = turn dir to
  let (row2, col2) = move dir2 count row col
  fn1 xs dir2 row2 col2

parse :: String -> (Char, Int)
parse s = (head s, read (drop 1 s) :: Int)

turn North turn =
  case turn of
    'R' -> East
    'L' -> West
turn West turn =
  case turn of
    'R' -> North
    'L' -> South
turn East turn =
  case turn of
    'R' -> South
    'L' -> North
turn South turn =
  case turn of
    'R' -> West
    'L' -> East

move North count row col = (row - count, col)
move South count row col = (row + count, col)
move East count row col = (row, col + count)
move West count row col = (row, col - count)
