-- | CRC32 utilities
module Codec.Circe.CRC32
  ( crc32Unsafe
  , crc32Table
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
{-
-- | calculate CRC32 using lookup table generated from config
crc16 :: CRC16Cfg -> ByteString -> Word16
crc16 cfg = crc16WithCfg t cfg
  where
    t = V.fromList $ crc16Table $ crcPoly cfg

-- | calculate CRC16 with extra configuration, table not calculated from poly.
crc16WithCfg :: Vector Word16 -> CRC16Cfg -> ByteString -> Word16
crc16WithCfg t cfg =
  xor (crcFinXor cfg)
    . applyWhen (crcRefOut cfg) ref16
    . crc16Unsafe t (crcRefIn cfg) (crcInit cfg)
-}
-- | calculate CRC32 using an efficient lookup table and init value
crc32Unsafe :: Vector Word32 -> Word32 -> ByteString -> Word32
crc32Unsafe t = BS.foldl' go
  where
    go crc b = crc `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
      where
        pos = fromIntegral ((crc `xor` (fromIntegral b `shiftL` 24)) `shiftR` 24) :: Word8

-- | generate lookup table using polynomial
crc32Table :: Word32 -> [Word32]
crc32Table poly = calc <$> [0..255]
  where
    calc = appEndo (stimes (8 :: Int) $ Endo step) . (`shiftL` 24)
    step curByte = applyWhen (testBit curByte 31) (`xor` poly) $ curByte `shiftL` 1
