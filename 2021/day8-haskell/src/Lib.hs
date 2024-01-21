module Lib
  ( fn1
  , fn2
  ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Inputs
import Lists
import Strings

fn1 :: [String] -> Int
fn1 lines = foldl (\sum entry -> sum + (v (digits entry))) 0 entries
  where
    entries = map toEntry lines
    v digits =
      foldl
        (\sum digit ->
           if length digit `elem` [2, 3, 4, 7]
             then sum + 1
             else sum)
        0
        digits

data Entry = Entry
  { signals :: [String]
  , digits :: [String]
  }

toEntry :: String -> Entry
toEntry s = Entry {signals = getStrings (newDelimiter a " "), digits = getStrings (newDelimiter b " ")}
  where
    del = newDelimiter s " | "
    a = getString del 0
    b = getString del 1

fn2 :: [String] -> Int
fn2 lines =  0
--
--calc :: Entry -> Int
--calc entry = 0
--  where
--    signal = calc1 (signals entry) $ SignalState "" "" [] [] "" ""
--    m = count [(one signal), (seven signal)]
--
--data SignalState = SignalState
--    { one    :: String
--    , four   :: String
--    , fives  :: [String]
--    , sixes  :: [String]
--    , seven  :: String
--    , eight  :: String
--    }
--
--calc1 :: [String] -> SignalState -> SignalState
--calc1 [] state = state
--calc1 (signal:xs) state = case length signal of
--    2 -> calc1 xs state { one = signal }
--    4 -> calc1 xs state { four = signal }
--    3 -> calc1 xs state { seven = signal }
--    7 -> calc1 xs state { eight = signal }
--    6 -> calc1 xs state { sixes = sixes state ++ [signal] }
--    5 -> calc1 xs state { fives = fives state ++ [signal] }
--    _ -> calc1 xs state
--
--count :: [String] -> Map.Map Char Int
--count sets = foldl (\m s -> (foldl (\m2 c -> Map.insertWith (+) c 1 m2) m s)) Map.empty sets
--
