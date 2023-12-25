import System.IO
import Data.List (isPrefixOf)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res

fn1 :: [String] -> Int
fn1 [] = 0
fn1 (x:xs) = fn1 xs

data Delimiter = Delimiter
  { ind :: [Int]
  , s :: String
  , del :: String
  }

newDelimiter :: String -> String -> Delimiter
newDelimiter s del =
  let indices = indexAll s del
   in Delimiter {ind = indices, s = s, del = del}

getStrings :: Delimiter -> [String]
getStrings d
  | null (ind d) = []
  | otherwise = getStrings' d 0

getStrings' :: Delimiter -> Int -> [String]
getStrings' d i
  | i == length (ind d) + 1 = []
  | otherwise = getString d i : getStrings' d (i + 1)

substring :: Int -> Int -> String -> String
substring start end = take (end - start) . drop start

getInt :: Delimiter -> Int -> Int
getInt d i = read (getString d i) :: Int

getInts :: Delimiter -> [Int]
getInts d = getInts' d (getStrings d)

getInts' :: Delimiter -> [String] -> [Int]
getInts' d [] = []
getInts' d (x:xs) = (read (x) :: Int) : getInts' d xs

getString :: Delimiter -> Int -> String
getString d i
  | i == 0 = substring 0 ((ind d) !! 0) (s d)
  | i == length (ind d) = substring (1 + (ind d) !! (length (ind d) - 1)) (length (s d)) (s d)
  | otherwise = substring (((ind d) !! (i - 1)) + length (del d)) ((ind d) !! i) (s d)

indexAll :: String -> String -> [Int]
indexAll s sep = indexAll' s sep 0

indexAll' :: String -> String -> Int -> [Int]
indexAll' [] _ _ = []
indexAll' all@(x:xs) sep idx
  | sep `isPrefixOf` all = idx : next
  | otherwise = next
  where
    next = indexAll' xs sep (idx + 1)
