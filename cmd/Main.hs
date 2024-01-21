import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import System.IO
import Text.Printf

totalStars = 450

main = do
  handle <- openFile "README.md" ReadMode
  content <- hGetContents handle
  let linesList = lines content
  let stats = count linesList
  forM_ (Map.toList stats) $ \(key, value) -> printf "%s: %.1f%%\n" key (fromIntegral value / totalStars * 100 :: Double)

count :: [String] -> Map.Map String Int
count lines = foldl updateStats stats lines
  where
    stats = Map.fromList [("Go", 0), ("Rust", 0), ("Haskell", 0), ("Python", 0)]

updateStats :: Map.Map String Int -> String -> Map.Map String Int
updateStats stats line = foldl (\acc language -> updateStat acc line language) stats (Map.keys stats)

updateStat :: Map.Map String Int -> String -> String -> Map.Map String Int
updateStat stats line language
  | isInfixOf ("[" ++ language ++ "]") line = Map.adjust (+ 2) language stats
  | isInfixOf ("[" ++ language ++ " ") line = Map.adjust (+ 1) language stats
  | otherwise = stats
