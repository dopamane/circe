module Codec.Circe.CRC16
  ( tableCRC16
  ) where

import Data.Bits
import Data.Function
import Data.Word

tableCRC16 :: Word16 -> [Word16]
tableCRC16 poly = [foldr go (dividend `shiftL` 8) [0..7] | dividend <- [0..255]]
  where
    go curByte _ = applyWhen (curByte .&. 0x8000 /= 0) (`xor` poly) $ curByte `shiftL` 1
