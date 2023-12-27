import Data.List (isPrefixOf)
import System.IO

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
--  let res = button "ULL"
  print res

fn1 :: [String] -> Int
fn1  = foldl (\acc x -> acc * 10 + (button x)) 0

button :: String -> Int
button = foldl (\acc x -> move (toMove x) acc) 5

toMove :: Char -> Move
toMove 'U' = MoveUp
toMove 'D' = MoveDown
toMove 'L' = MoveLeft
toMove 'R' = MoveRight

move :: Move -> Int -> Int
move MoveUp 1 = 1
move MoveUp 2 = 2
move MoveUp 3 = 3
move MoveUp 4 = 1
move MoveUp 5 = 2
move MoveUp 6 = 3
move MoveUp 7 = 4
move MoveUp 8 = 5
move MoveUp 9 = 6
move MoveDown 1 = 4
move MoveDown 2 = 5
move MoveDown 3 = 6
move MoveDown 4 = 7
move MoveDown 5 = 8
move MoveDown 6 = 9
move MoveDown 7 = 7
move MoveDown 8 = 8
move MoveDown 9 = 9
move MoveLeft 1 = 1
move MoveLeft 2 = 1
move MoveLeft 3 = 2
move MoveLeft 4 = 4
move MoveLeft 5 = 4
move MoveLeft 6 = 5
move MoveLeft 7 = 7
move MoveLeft 8 = 7
move MoveLeft 9 = 8
move MoveRight 1 = 2
move MoveRight 2 = 3
move MoveRight 3 = 3
move MoveRight 4 = 5
move MoveRight 5 = 6
move MoveRight 6 = 6
move MoveRight 7 = 8
move MoveRight 8 = 9
move MoveRight 9 = 9
