module Conversions
  ( binaryToDecimal
  ) where

import Data.Char (digitToInt)
import Numeric (readInt)

binaryToDecimal :: String -> Int
binaryToDecimal binaryString =
  case readInt 2 (`elem` "01") digitToInt binaryString of
    [(decimalValue, _)] -> decimalValue
    _ -> error "Invalid binary string"
