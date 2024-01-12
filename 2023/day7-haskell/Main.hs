import Data.List (isPrefixOf, sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Debug.Trace
import Lib.Inputs
import Lib.Strings
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

data CardValue
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | As
  deriving (Eq, Ord, Show, Read)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

data Player = Player
  { hand :: [CardValue]
  , handType :: HandType
  , bet :: Int
  } deriving (Show)

fn1 :: [String] -> Int
fn1 lines = res 1 players
  where
    players = sortBy comparePlayers (map toPlayer lines)
    comparePlayers :: Player -> Player -> Ordering
    comparePlayers p1 p2 = compare (handType p1, hand p1) (handType p2, hand p2)
    compareHands :: [CardValue] -> [CardValue] -> Ordering
    compareHands [] [] = EQ
    compareHands (x:xs) (y:ys) =
      case compare x y of
        EQ -> compareHands xs ys
        result -> result

res :: Int -> [Player] -> Int
res i [] = 0
res i (x:xs) = (bet x) * i + res (i + 1) xs

toPlayer :: String -> Player
toPlayer s = Player {hand = hand, handType = handType, bet = bet}
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
  | isCount m 5 = FiveOfAKind
  | isCount m 4 = FourOfAKind
  | isCount m 3 && isCount m 2 = FullHouse
  | isCount m 3 = ThreeOfAKind
  | isTwoPair m = TwoPair
  | isCount m 2 = OnePair
  | otherwise = HighCard
  where
    m =
      foldl
        (\acc a -> Map.insertWith (+) a 1 acc)
        (Map.empty :: Map.Map CardValue Int)
        cards
    isCount :: Map.Map CardValue Int -> Int -> Bool
    isCount m value = any (\(_, frequency) -> frequency == value) (Map.toList m)
    isTwoPair :: Map.Map CardValue Int -> Bool
    isTwoPair m =
      sum
        (Map.map
           (\frequency ->
              if frequency == 2
                then 1
                else 0)
           m)
        == 2

fn2 :: [String] -> Int
fn2 lines = res 1 players
  where
    players = sortBy comparePlayers (map toPlayer2 lines)
    comparePlayers :: Player -> Player -> Ordering
    comparePlayers p1 p2
      | (handType p1) /= (handType p2) = compare (handType p1) (handType p2)
      | otherwise = compareHands (hand p1) (hand p2)
    compareHands :: [CardValue] -> [CardValue] -> Ordering
    compareHands [] [] = EQ
    compareHands (x:xs) (y:ys)
      | compare x y == EQ = compareHands xs ys
      | x == Jack = LT
      | y == Jack = GT
      | otherwise = compare x y

toPlayer2 :: String -> Player
toPlayer2 s = Player {hand = hand, handType = handType, bet = bet}
  where
    del = newDelimiter s " "
    hand = map toCardValue $ getString del 0
    jFrequency = length . filter (== 'J') $ getString del 0
    handType = toHandType2 hand jFrequency
    bet = getInt del 1

toHandType2 :: [CardValue] -> Int -> HandType
toHandType2 cards jFrequency
  | isCount m 5 = FiveOfAKind
  | isCount m 4 = FourOfAKind
  | isCount m 3 && isCount m 2 = FullHouse
  | isCount m 3 = ThreeOfAKind
  | isTwoPair m = TwoPair
  | isCount m 2 = OnePair
  | otherwise = HighCard
  where
    init =
      foldl
        (\acc a -> Map.insertWith (+) a 1 acc)
        (Map.empty :: Map.Map CardValue Int)
        cards
    highestCard = getBestCard (Map.toList init) Two 0
    m = Map.insertWith (+) highestCard jFrequency init
    isCount :: Map.Map CardValue Int -> Int -> Bool
    isCount m value =
      any
        (\(card, frequency) -> card /= Jack && frequency == value)
        (Map.toList m)
    isTwoPair :: Map.Map CardValue Int -> Bool
    isTwoPair m =
      sum
        (map
           (\(card, frequency) ->
              if card == Jack
                then 0
                else (if frequency == 2
                        then 1
                        else 0))
           (Map.toList m))
        == 2

getBestCard :: [(CardValue, Int)] -> CardValue -> Int -> CardValue
getBestCard [] highest _ = highest
getBestCard ((Jack, freq):rem) highest best = getBestCard rem highest best
getBestCard ((card, freq):rem) highest best
  | freq > best = getBestCard rem card freq
  | freq < best = getBestCard rem highest best
  | otherwise = getBestCard rem (max card highest) freq
