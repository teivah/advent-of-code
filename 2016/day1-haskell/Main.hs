import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace
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
  let map = Map.empty
  let res = fn2 map s North 0 0 0 0
  print res

fn1 :: [String] -> Direction -> Int -> Int -> Int
fn1 [] _ row col = abs row + abs col
fn1 (x:xs) dir row col = do
  let (to, count) = parse x
  let dir2 = turn dir to
  let (row2, col2) = move dir2 count row col
  fn1 xs dir2 row2 col2

fn2 :: Map.Map (Int, Int) Bool -> [String] -> Direction -> Int -> Int -> Int -> Int -> Int
fn2 _ [] _ _ _ 0 0 = error "no result"
fn2 map [] dir row col remainingRow remainingCol =
  let v = Map.lookup (row, col) map
   in if v /= Nothing
        then abs row + abs col
        else do
          let map2 = Map.insert (row, col) True map
          let (row2, col2, deltaRow2, deltaCol2) = moveOne row col remainingRow remainingCol
          fn2 map2 [] dir row2 col2 deltaRow2 deltaCol2
fn2 map (x:xs) dir row col remainingRow remainingCol =
  let v = Map.lookup (row, col) map
   in if v /= Nothing
        then abs row + abs col
        else if remainingRow /= 0 || remainingCol /= 0
               then do
                 let map2 = Map.insert (row, col) True map
                 let (row2, col2, deltaRow2, deltaCol2) = moveOne row col remainingRow remainingCol
                 fn2 map2 (x : xs) dir row2 col2 deltaRow2 deltaCol2
               else do
                 let map2 = Map.insert (row, col) True map
                 let (to, count) = parse x
                 let dir2 = turn dir to
                 let (deltaRow, deltaCol) = delta dir2 count
                 let (row2, col2, deltaRow2, deltaCol2) = moveOne row col deltaRow deltaCol
                 fn2 map2 (xs) dir2 row2 col2 deltaRow2 deltaCol2

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

delta North count = (-count, 0)
delta South count = (count, 0)
delta East count = (0, count)
delta West count = (0, -count)

moveOne :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
moveOne row col deltaRow deltaCol
  | deltaRow > 0 = (row + 1, col, deltaRow - 1, 0)
  | deltaRow < 0 = (row - 1, col, deltaRow + 1, 0)
  | deltaCol > 0 = (row, col + 1, 0, deltaCol - 1)
  | otherwise = (row, col - 1, 0, deltaCol + 1)
