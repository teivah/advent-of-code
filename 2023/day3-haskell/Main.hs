import System.IO
import qualified Lib.Inputs
import Debug.Trace

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList linesList 0
  print res

fn1 :: [String] -> [String] -> Int -> Int
fn1 _ [] _ = 0
fn1 all (x:xs) row = parseLine all x (Position row 0) False (Position row 0) + fn1 all xs (row + 1)

data Position = Position Int Int deriving (Show)

parseLine :: [String] -> String -> Position -> Bool -> Position -> Int
parseLine _ [] _ False _ = 0
parseLine all [] (Position row col) True p = check all p (Position row $ col - 1)
parseLine all (x:xs) (Position row col) False p
  | x >= '0' && x <= '9' = parseLine all xs (Position row $ col + 1) True (Position row col)
  | otherwise = parseLine all xs (Position row $ col + 1) False p
parseLine all (x:xs) (Position row col) True p
  | x >= '0' && x <= '9' = parseLine all xs (Position row $ col + 1) True p
  | otherwise = check all p (Position row $ col - 1) +
    parseLine all xs (Position row $ col + 1) False (Position 0 0)

check :: [String] -> Position -> Position -> Int
check all (Position fromRow fromCol) (Position toRow toCol) =
--  trace (show (number, found)) $
  if found then number else 0
  where
    number = parseNumber all (Position fromRow fromCol) (Position toRow toCol)
    found = (containsSign all (Position fromRow $ fromCol - 1)) ||
              (containsSign all (Position fromRow $ toCol + 1)) ||
              (containsSignRange all (Position (fromRow - 1) (fromCol -1)) (Position (fromRow - 1) (toCol + 1))) ||
              (containsSignRange all (Position (fromRow + 1) (fromCol -1)) (Position (fromRow + 1) (toCol + 1)))

containsSignRange :: [String] -> Position -> Position -> Bool
containsSignRange all (Position row col) (Position toRow toCol)
  | col > toCol = False
  | otherwise =
--    trace (show (row, col)) $
    if containsSign all (Position row col) then True else
    containsSignRange all (Position row $ col + 1) (Position toRow toCol)

containsSign :: [String] -> Position -> Bool
containsSign all (Position row col) =
  if not inside then False else
    let v = all !! row !! col
    in v /= '.'
  where inside = row >= 0 && col >= 0 && row < length all && col < length (all !! 0)

parseNumber :: [String] -> Position -> Position -> Int
parseNumber all from to =
--  trace (show (numbers, from, to)) $
  foldl (\acc a -> acc * 10 + a) 0 numbers
  where numbers = parseNumber' all from to

parseNumber' :: [String] -> Position -> Position -> [Int]
parseNumber' all (Position row col) (Position toRow toCol)
  | col > toCol = []
  | otherwise =
--    trace (show (all, row, col, toRow, toCol)) $
    fromEnum (all !! row !! col) - fromEnum '0' : parseNumber' all (Position row $ col + 1) (Position toRow toCol)

