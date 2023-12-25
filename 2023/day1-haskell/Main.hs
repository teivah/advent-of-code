import Data.Char (digitToInt)
import Data.List (isPrefixOf)
import System.IO

main :: IO ()
main = do
  res <- withFile "input.txt" ReadMode (\handle -> fn1 handle 0)
  print res
  res <- withFile "input.txt" ReadMode (\handle -> fn2 handle 0)
  print res
  let res = firstDigitLetter "xwadaww" False 0 0
  print res

fn1 handle sum = do
  eof <- hIsEOF handle
  if eof
    then return sum
    else do
      line <- hGetLine handle
      let v1 = firstDigit line
      let v2 = firstDigit (reverse line)
      fn1 handle (sum + v1 * 10 + v2)

firstDigit [] = error "no result"
firstDigit (x:xs) =
  if x >= '0' && x <= '9'
    then digitToInt x
    else firstDigit xs

fn2 handle sum = do
  eof <- hIsEOF handle
  if eof
    then return sum
    else do
      line <- hGetLine handle
      let v = firstDigitLetter line False 0 0
      fn2 handle (sum + v)

firstDigitLetter [] _ v1 0 = v1 * 10 + v1
firstDigitLetter [] _ v1 v2 = v1 * 10 + v2
firstDigitLetter all@(_:xs) found v1 v2 =
  if v /= 0
    then if found
           then firstDigitLetter xs True v1 v
           else firstDigitLetter xs True v 0
    else firstDigitLetter xs found v1 v2
  where
    v = startsWith all

startsWith str
  | isDigit = digitToInt x
  | "one" `isPrefixOf` str = 1
  | "two" `isPrefixOf` str = 2
  | "three" `isPrefixOf` str = 3
  | "four" `isPrefixOf` str = 4
  | "five" `isPrefixOf` str = 5
  | "six" `isPrefixOf` str = 6
  | "seven" `isPrefixOf` str = 7
  | "eight" `isPrefixOf` str = 8
  | "nine" `isPrefixOf` str = 9
  | otherwise = 0
  where
    (x:_) = str
    isDigit = x >= '1' && x <= '9'
