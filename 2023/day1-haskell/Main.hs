import Data.Char (digitToInt)
import Data.List (isPrefixOf)
import System.IO

main :: IO ()
main = do
  res <- withFile "input.txt" ReadMode (\handle -> fn1 handle 0)
  print res

--  res <- withFile "input.txt" ReadMode (\handle -> fn2 handle 0)
--  print res
fn1 handle sum = do
  eof <- hIsEOF handle
  if eof
    then return sum
    else do
      line <- hGetLine handle
      let v1 = firstDigit line
      let v2 = firstDigit (reverse line)
      fn1 handle (sum + v1 * 10 + v2)

firstDigit :: String -> Int
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
      let v1 = firstDigitLetter line
      print v1
      fn2 handle 0

--      fn1 handle (sum + v1 * 10 + v2)
firstDigitLetter :: String -> Int
firstDigitLetter [] = error "no result"
firstDigitLetter all@(x:xs) =
  if x >= '0' && x <= '9'
    then digitToInt x
    else firstDigit xs

startsWith str
  | "one" `isPrefixOf` str = 1
  | otherwise = 0
