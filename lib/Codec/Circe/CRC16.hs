-- | CRC16 utilities
module Codec.Circe.CRC16
  ( crc16Table
  , crc16Unsafe
  , crc16WithCfg
  , reflect
  ) where

import Codec.Circe.Cfg
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
crc16Table poly = calc <$> [0..255]
  where
    calc = appEndo (stimes (8 :: Int) $ Endo step) . (`shiftL` 8)
    step curByte = applyWhen (testBit curByte 15) (`xor` poly) $ curByte `shiftL` 1

-- | calculate CRC16 using an efficient lookup table and init value
crc16Unsafe :: Vector Word16 -> Word16 -> ByteString -> Word16
crc16Unsafe t = BS.foldl' go
  where
    go crc b = crc `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
      where
        pos = fromIntegral (crc `shiftR` 8) `xor` b

-- | calculate CRC16 with extra configuration, table not calculated from poly.
crc16WithCfg :: Vector Word16 -> CRC16Cfg -> ByteString -> Word16
crc16WithCfg t cfg =
  xor (crcFinXor cfg)
    . applyWhen (crcRefOut cfg) reflect
    . crc16Unsafe t (crcInit cfg)
    . applyWhen (crcRefIn cfg) (BS.map reflect)

reflect :: FiniteBits a => a -> a
reflect a = getIor $ foldMap go [0 .. maxIdx]
  where
    maxIdx = finiteBitSize a
    go idx | testBit a idx = Ior $ bit $ maxIdx - idx
           | otherwise     = mempty
