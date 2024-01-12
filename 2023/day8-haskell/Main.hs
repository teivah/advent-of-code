import Data.List (isPrefixOf)
import Debug.Trace
import Lib.Inputs
import Lib.Strings
import Lib.Directions
import System.IO
import qualified Data.Map as Map

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res
  let res = fn2 linesList
  print res

fn1 :: [String] -> Int
fn1 [] = 0
fn1 lines = find directions nodes 0 "AAA"
  where
    groups = stringGroups lines
    directions = parseDirections (groups !! 0 !! 0)
    nodes = parseNodes (groups !! 1)

find :: [Direction] -> Map.Map String Node -> Int -> String -> Int
find _ _ _ "ZZZ" = 0
find directions nodes i cur = 1 + find directions nodes j next
  where
     dir = directions !! i
     j = (i+1) `mod` length directions
     next = case Map.lookup cur nodes of
       Just node -> if dir == DirLeft then left node else right node
       Nothing -> error $ "node not found: " ++ cur

parseDirections :: String -> [Direction]
parseDirections s = map (\c -> if c == 'L' then DirLeft else DirRight) s

data Node = Node {left :: String, right :: String}
  deriving (Show)

parseNodes :: [String] -> Map.Map String Node
parseNodes lines = foldl func (Map.empty :: Map.Map String Node) lines
  where
    func acc a =
      let (node, s) = parseNode a
      in Map.insert s node acc

parseNode :: String -> (Node, String)
parseNode s = (Node{left= getString directions 0, right= getString directions 1}, name)
  where
    del = newDelimiter s " = "
    name = getString del 0
    tmp = getString del 1
    nodes = tail (init (getString del 1))
    directions = newDelimiter nodes ", "

fn2 :: [String] -> Int
fn2 [] = 0
fn2 lines = trace (show (distances)) $ foldl lcm 1 numbers
  where
    groups = stringGroups lines
    directions = parseDirections (groups !! 0 !! 0)
    nodes = parseNodes (groups !! 1)
    curs = map fst $ filter (\(name, v) -> isStartingNode name) (Map.toList nodes)
    distances = find2 0 directions (Map.empty :: Map.Map Int Int) nodes 0 curs
    numbers = Map.elems distances

find2 :: Int -> [Direction] -> Map.Map Int Int -> Map.Map String Node -> Int -> [String] -> Map.Map Int Int
find2 distance directions distances nodes i curs
  | length distances == length curs = distances
  | otherwise = find2 (distance + 1) directions nextDistances nodes j next
    where
      dir = directions !! i
      next = traverseCurs curs dir nodes
      nextDistances = setDistance (distance + 1) 0 curs distances
      j = (i+1) `mod` length directions

traverseCurs :: [String] -> Direction -> Map.Map String Node -> [String]
traverseCurs [] _ _ = []
traverseCurs (x:xs) dir nodes = next : traverseCurs xs dir nodes
  where
   next = case Map.lookup x nodes of
     Just node -> if dir == DirLeft then left node else right node
     Nothing -> error "value not found"

setDistance :: Int -> Int -> [String] -> Map.Map Int Int -> Map.Map Int Int
setDistance _ _ [] distances = distances
setDistance distance i (x:xs) distances =
  setDistance distance (i+1) xs next
  where
    next
      | isEndingNode x || not (Map.member i distances) = Map.insert i distance distances

isStartingNode :: String -> Bool
isStartingNode name = (name !! (length name - 1)) == 'A'

isEndingNode :: String -> Bool
isEndingNode name = (name !! (length name - 1)) == 'Z'
