import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Debug.Trace
import Lib.Directions
import Lib.Inputs
import Lib.Lists
import Lib.Strings
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res

data Line = Line
  { from :: Position
  , to :: Position
  , direction :: Direction
  } deriving (Eq, Show)

fn1 :: [String] -> Int
fn1 s = length $ filter (\(_, v) -> v >= 2) (Map.toList grid)
  where
    lines = map toLine s
    grid = toGrid lines Map.empty

toGrid :: [Line] -> Map.Map Position Int -> Map.Map Position Int
toGrid [] grid = grid
toGrid (line:xs) grid =
  case d of
    Just (deltaRow, deltaCol) ->
      let grid2 = Map.insertWith (+) (from line) 1 grid
       in toGrid xs $ updateGrid (deltaRow, deltaCol) grid2 (from line) (to line)
    Nothing -> toGrid xs grid
  where
    d = deltaDir (direction line)
    updateGrid :: (Int, Int) -> Map.Map Position Int -> Position -> Position -> Map.Map Position Int
    updateGrid (deltaRow, deltaCol) grid cur to
      | cur == to = grid
      | otherwise = updateGrid (deltaRow, deltaCol) (Map.insertWith (+) cur 1 grid) (delta cur deltaRow deltaCol) to

deltaDir :: Direction -> Maybe (Int, Int)
deltaDir DirUp = Just (-1, 0)
deltaDir DirDown = Just (1, 0)
deltaDir DirLeft = Just (0, -1)
deltaDir DirRight = Just (0, 1)
deltaDir _ = Nothing

toLine :: String -> Line
toLine s = Line {from = from, to = to, direction = direction}
  where
    del = newDelimiter s " "
    f = getString del 0
    delF = newDelimiter f ","
    from = newPosition (getInt delF 1) (getInt delF 0)
    t = getString del 2
    delT = newDelimiter t ","
    to = newPosition (getInt delT 1) (getInt delT 0)
    direction
      | row from == row to =
        if col from < col to
          then DirRight
          else DirLeft
      | col from == col to =
        if row from < row to
          then DirDown
          else DirUp
      | col from < col to && row from < row to = DirDownRight
      | col from < col to && row from > row to = DirUpRight
      | col from > col to && row from < row to = DirDownLeft
      | col from > col to && row from > row to = DirUpLeft
