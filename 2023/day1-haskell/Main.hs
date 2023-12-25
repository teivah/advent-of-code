import Data.Char (digitToInt)
import Data.List (isPrefixOf)
import System.IO

main :: IO ()
main = do
  res <- withFile "input.txt" ReadMode (\handle -> fn handle 0 usingDigit)
  print res
  res <- withFile "input.txt" ReadMode (\handle -> fn handle 0 usingDigitAndWord)
  print res

fn handle sum f = do
  eof <- hIsEOF handle
  if eof
    then return sum
    else do
      line <- hGetLine handle
      let v = f line
      fn handle (sum + v) f

usingDigit line = (firstDigit line) * 10 + (firstDigit (reverse line))

firstDigit [] = error "no result"
firstDigit (x:xs)
  | x >= '0' && x <= '9' = digitToInt x
  | otherwise = firstDigit xs

usingDigitAndWord line = firstDigitLetter line False 0 0

firstDigitLetter [] _ v1 0 = v1 * 10 + v1
firstDigitLetter [] _ v1 v2 = v1 * 10 + v2
firstDigitLetter all@(_:xs) found v1 v2
  | v /= 0 =
    if found
      then firstDigitLetter xs True v1 v
      else firstDigitLetter xs True v 0
  | otherwise = firstDigitLetter xs found v1 v2
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
