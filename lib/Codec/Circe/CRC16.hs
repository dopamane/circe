-- | CRC16 utilities
module Codec.Circe.CRC16
  ( tableCRC16
  , crc16
  ) where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Function
import Data.Word

-- | generate lookup table using polynomial
tableCRC16 :: Word16 -> [Word16]
tableCRC16 poly = [foldr go (dividend `shiftL` 8) [0..7] | dividend <- [0..255]]
  where
    go :: Word8 -> Word16 -> Word16
    go _ curByte = applyWhen (testBit curByte 15) (`xor` poly) $ curByte `shiftL` 1

-- | calculate CRC16 using an efficient lookup table
crc16 :: Vector Word16 -> Word16 -> ByteString -> Word16
crc16 t = BS.foldl' $ \crc b ->
  let pos = fromIntegral (crc `shiftR` 8) `xor` b
  in crc `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
