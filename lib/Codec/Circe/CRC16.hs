-- | CRC16 utilities
module Codec.Circe.CRC16
  ( crc16
  , crc16WithTable
  , crc16Unsafe
  , crc16Table
  ) where

import Codec.Circe.Cfg
import Codec.Circe.Reflect
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Function
import Data.Monoid
import Data.Semigroup
import Data.Word

-- | calculate CRC16 using lookup table generated from config
crc16 :: CRC16Cfg -> ByteString -> Word16
crc16 cfg = crc16WithTable t cfg
  where
    t = V.fromList $ crc16Table $ crcPoly cfg

-- | calculate CRC16 with precalculated poly table
crc16WithTable :: Vector Word16 -> CRC16Cfg -> ByteString -> Word16
crc16WithTable t cfg =
  xor (crcFinXor cfg)
    . applyWhen (crcRefOut cfg) ref16
    . crc16Unsafe t (crcRefIn cfg) (crcInit cfg)

-- | calculate CRC16 using an efficient lookup table and init value
crc16Unsafe :: Vector Word16 -> Bool -> Word16 -> ByteString -> Word16
crc16Unsafe t refIn = BS.foldl' go
  where
    go crc b = crc `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
      where
        pos = fromIntegral (crc `shiftR` 8) `xor` applyWhen refIn ref8 b

-- | generate lookup table using polynomial
crc16Table :: Word16 -> [Word16]
crc16Table poly = calc <$> [0..255]
  where
    calc = appEndo (stimes (8 :: Int) $ Endo step) . (`shiftL` 8)
      where
        step curByte = applyWhen (testBit curByte 15) (`xor` poly) $ curByte `shiftL` 1
