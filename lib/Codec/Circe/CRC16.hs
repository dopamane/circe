-- | CRC16 utilities
module Codec.Circe.CRC16
  ( crc16
  , crc16Table
  ) where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Function
import Data.Monoid
import Data.Semigroup
import Data.Word

-- | generate lookup table using polynomial
crc16Table :: Word16 -> [Word16]
crc16Table poly = [appEndo e $ dividend `shiftL` 8 | dividend <- [0..255]]
  where
    e :: Endo Word16
    e = stimes (8 :: Int) $ Endo $ \curByte -> applyWhen (testBit curByte 15) (`xor` poly) $ curByte `shiftL` 1

-- | calculate CRC16 using an efficient lookup table and init value
crc16 :: Vector Word16 -> Word16 -> ByteString -> Word16
crc16 t = BS.foldl' $ \crc b ->
  let pos = fromIntegral (crc `shiftR` 8) `xor` b
  in crc `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
