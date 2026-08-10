module Codec.Circe.CRC16
  ( tableCRC16
  ) where

import Data.Bits
import Data.Function
import Data.Word
import Numeric

-- | generate lookup table using polynomial
tableCRC16 :: Word16 -> [Word16]
tableCRC16 poly = [foldr go (dividend `shiftL` 8) [0..7] | dividend <- [0..255]]
  where
    go :: Word8 -> Word16 -> Word16
    go _ curByte = applyWhen (testBit curByte 15) (`xor` poly) $ curByte `shiftL` 1
