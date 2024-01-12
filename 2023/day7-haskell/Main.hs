import Data.List (isPrefixOf, sortBy)
import Data.Ord (comparing)
import Debug.Trace
import Lib.Inputs
import Lib.Strings
import System.IO
import qualified Data.Map as Map

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let linesList = lines contents
  let res = fn1 linesList
  print res

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | As
  deriving (Eq, Ord, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Ord, Show)

data Player = Player
  { hand :: [CardValue]
  , handType :: HandType
  , bet :: Int
  } deriving (Show)

comparePlayers :: Player -> Player -> Ordering
comparePlayers p1 p2
  | handType p1 /= handType p2 = compare (handType p1) (handType p2)
  | otherwise = compareHands (hand p1) (hand p2)

compareHands :: [CardValue] -> [CardValue] -> Ordering
compareHands [] [] = EQ
compareHands (x:xs) (y:ys) = case compare x y of
  EQ -> compareHands xs ys
  result -> result

fn1 :: [String] -> Int
fn1 [] = 0
fn1 lines = (trace (show (players))) $ res 1 players
  where
    players = sortBy comparePlayers (map toPlayer lines)

res :: Int -> [Player] -> Int
res i [] = 0
res i (x:xs) = (bet x) * i + res (i + 1) xs

toPlayer :: String -> Player
toPlayer s = Player {hand=hand, handType = handType, bet = bet}
  where
    del = newDelimiter s " "
    hand = map toCardValue $ getString del 0
    handType = toHandType hand
    bet = getInt del 1

toCardValue :: Char -> CardValue
toCardValue '2' = Two
toCardValue '3' = Three
toCardValue '4' = Four
toCardValue '5' = Five
toCardValue '6' = Six
toCardValue '7' = Seven
toCardValue '8' = Eight
toCardValue '9' = Nine
toCardValue 'T' = Ten
toCardValue 'J' = Jack
toCardValue 'Q' = Queen
toCardValue 'K' = King
toCardValue 'A' = As

toHandType :: [CardValue] -> HandType
toHandType cards
  | isCount map 5 = FiveOfAKind
  | isCount map 4 = FourOfAKind
  | isCount map 3 && isCount map 2 = FullHouse
  | isCount map 3 = ThreeOfAKind
  | isTwoPair map = TwoPair
  | isCount map 2 = OnePair
  | otherwise = HighCard
  where
    map = foldl (\acc a -> Map.insertWith (+) a 1 acc) (Map.empty :: Map.Map CardValue Int) cards
    isCount :: Map.Map CardValue Int -> Int -> Bool
    isCount m value = any (\(_, frequency) -> frequency == value) (Map.toList m)
    isTwoPair :: Map.Map CardValue Int -> Bool
    isTwoPair m = sum (Map.map (\frequency -> if frequency == 2 then 1 else 0) m) == 2


