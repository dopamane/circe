-- | Reflect bits
module Codec.Circe.Reflect (ref32, ref16, ref8) where

import Data.Bits
import Data.Word

-- | Reflect 'Word32'
ref32 :: Word32 -> Word32
ref32 = s1 . s2 . s4 . s8 . s16
  where
    s1 n = ((n .&. 0xAAAAAAAA) `shiftR` 1) .|. ((n .&. 0x55555555) `shiftL` 1)
    s2 n = ((n .&. 0xCCCCCCCC) `shiftR` 2) .|. ((n .&. 0x33333333) `shiftL` 2)
    s4 n = ((n .&. 0xF0F0F0F0) `shiftR` 4) .|. ((n .&. 0x0F0F0F0F) `shiftL` 4)
    s8 n = ((n .&. 0xFF00FF00) `shiftR` 8) .|. ((n .&. 0x00FF00FF) `shiftL` 8)
    s16 n = (n `shiftR` 16) .|. (n `shiftL` 16)

-- | Reflect 'Word16'
ref16 :: Word16 -> Word16
ref16 = s1 . s2 . s4 . s8
  where
    s1 n = ((n .&. 0xAAAA) `shiftR` 1) .|. ((n .&. 0x5555) `shiftL` 1)
    s2 n = ((n .&. 0xCCCC) `shiftR` 2) .|. ((n .&. 0x3333) `shiftL` 2)
    s4 n = ((n .&. 0xF0F0) `shiftR` 4) .|. ((n .&. 0x0F0F) `shiftL` 4)
    s8 n = (n `shiftR` 8) .|. (n `shiftL` 8)

-- | Reflect 'Word8'
ref8 :: Word8 -> Word8
ref8 = s1 . s2 . s4
  where
    s1 n = ((n .&. 0xAA) `shiftR` 1) .|. ((n .&. 0x55) `shiftL` 1)
    s2 n = ((n .&. 0xCC) `shiftR` 2) .|. ((n .&. 0x33) `shiftL` 2)
    s4 n = (n `shiftR` 4) .|. (n `shiftL` 4)
