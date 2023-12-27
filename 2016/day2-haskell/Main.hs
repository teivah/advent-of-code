import Data.List (isPrefixOf)
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res
  let res = fn2 linesList
  print res

fn1 :: [String] -> String
fn1  = foldl (\acc x -> acc ++ [button move x]) ""

fn2 :: [String] -> String
fn2  = foldl (\acc x -> acc ++ [button move2 x]) ""

button :: (Char -> Char -> Char) -> String -> Char
button f = foldl (\acc x -> f x acc) '5'

move :: Char -> Char -> Char
move 'U' '1' = '1'
move 'U' '2' = '2'
move 'U' '3' = '3'
move 'U' '4' = '1'
move 'U' '5' = '2'
move 'U' '6' = '3'
move 'U' '7' = '4'
move 'U' '8' = '5'
move 'U' '9' = '6'
move 'D' '1' = '4'
move 'D' '2' = '5'
move 'D' '3' = '6'
move 'D' '4' = '7'
move 'D' '5' = '8'
move 'D' '6' = '9'
move 'D' '7' = '7'
move 'D' '8' = '8'
move 'D' '9' = '9'
move 'L' '1' = '1'
move 'L' '2' = '1'
move 'L' '3' = '2'
move 'L' '4' = '4'
move 'L' '5' = '4'
move 'L' '6' = '5'
move 'L' '7' = '7'
move 'L' '8' = '7'
move 'L' '9' = '8'
move 'R' '1' = '2'
move 'R' '2' = '3'
move 'R' '3' = '3'
move 'R' '4' = '5'
move 'R' '5' = '6'
move 'R' '6' = '6'
move 'R' '7' = '8'
move 'R' '8' = '9'
move 'R' '9' = '9'

move2 :: Char -> Char -> Char
move2 'U' '1' = '1'
move2 'U' '2' = '2'
move2 'U' '3' = '1'
move2 'U' '4' = '4'
move2 'U' '5' = '5'
move2 'U' '6' = '2'
move2 'U' '7' = '3'
move2 'U' '8' = '4'
move2 'U' '9' = '9'
move2 'U' 'A' = '6'
move2 'U' 'B' = '7'
move2 'U' 'C' = '8'
move2 'U' 'D' = 'B'
move2 'D' '1' = '3'
move2 'D' '2' = '6'
move2 'D' '3' = '7'
move2 'D' '4' = '8'
move2 'D' '5' = '5'
move2 'D' '6' = 'A'
move2 'D' '7' = 'B'
move2 'D' '8' = 'C'
move2 'D' '9' = '9'
move2 'D' 'A' = 'A'
move2 'D' 'B' = 'D'
move2 'D' 'C' = 'C'
move2 'D' 'D' = 'D'
move2 'L' '1' = '1'
move2 'L' '2' = '2'
move2 'L' '3' = '2'
move2 'L' '4' = '3'
move2 'L' '5' = '5'
move2 'L' '6' = '5'
move2 'L' '7' = '6'
move2 'L' '8' = '7'
move2 'L' '9' = '8'
move2 'L' 'A' = 'A'
move2 'L' 'B' = 'A'
move2 'L' 'C' = 'B'
move2 'L' 'D' = 'D'
move2 'R' '1' = '1'
move2 'R' '2' = '3'
move2 'R' '3' = '4'
move2 'R' '4' = '4'
move2 'R' '5' = '6'
move2 'R' '6' = '7'
move2 'R' '7' = '8'
move2 'R' '8' = '9'
move2 'R' '9' = '9'
move2 'R' 'A' = 'B'
move2 'R' 'B' = 'C'
move2 'R' 'C' = 'C'
move2 'R' 'D' = 'D'
