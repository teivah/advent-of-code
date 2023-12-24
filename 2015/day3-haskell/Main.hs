import qualified Data.Map as Map
import System.IO

main :: IO ()
main = do
  content <- readFile "input.txt"
  let map = Map.empty
  let res = fn1 content map (0, 0)
  print res
  let res = fn2 content map (0, 0) (0, 0) True
  print res

fn1 [] map (_, _) = Map.size map
fn1 (x:xs) map (row, col) = fn1 xs map2 (row2, col2)
  where
    map2 = Map.insert (row, col) True map
    (row2, col2) = transform x row col

fn2 [] map (_, _) (_, _) _ = Map.size map
fn2 (x:xs) map (sRow, sCol) (rRow, rCol) sTurn =
  fn2 xs map2 (sRow2, sCol2) (rRow2, rCol2) sTurn2
  where
    map2 =
      if sTurn
        then Map.insert (sRow, sCol) True map
        else Map.insert (rRow, rCol) True map
    (sRow2, sCol2) =
      if sTurn
        then transform x sRow sCol
        else (sRow, sCol)
    (rRow2, rCol2) =
      if not sTurn
        then transform x rRow rCol
        else (rRow, rCol)
    sTurn2 = not sTurn

transform x row col =
  case x of
    '^' -> (row + 1, col)
    'v' -> (row - 1, col)
    '<' -> (row, col - 1)
    '>' -> (row, col + 1)
