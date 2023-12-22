import Data.List (sort)
import Data.List.Split (splitOn)
import System.IO

main :: IO ()
main = do
  res <- withFile "input.txt" ReadMode (\handle -> fn1 handle 0)
  print res
  res <- withFile "input.txt" ReadMode (\handle -> fn2 handle 0)
  print res

fn1 handle count = do
  eof <- hIsEOF handle
  if eof
    then return count
    else do
      line <- hGetLine handle
      let [l, w, h] = parse line
      fn1
        handle
        (count + 2 * l * w + 2 * w * h + 2 * h * l + smallestSide [l, w, h])

parse :: String -> [Int]
parse input = map read (splitOn "x" input)

smallestSide dimensions =
  let [x, y, _] = sort dimensions
   in x * y

fn2 handle count = do
  eof <- hIsEOF handle
  if eof
    then return count
    else do
      line <- hGetLine handle
      let [l, w, h] = parse line
      let [x, y, _] = sort [l, w, h]
      fn2 handle (count + 2 * x + 2 * y + l * w * h)
