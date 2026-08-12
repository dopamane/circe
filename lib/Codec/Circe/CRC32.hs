-- | CRC32 utilities
module Codec.Circe.CRC32
  ( crc32
  , crc32WithCfg
  , crc32Unsafe
  , crc32Table
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

-- | calculate CRC32 using lookup table generated from config
crc32 :: CRC32Cfg -> ByteString -> Word32
crc32 cfg = crc32WithCfg t cfg
  where
    t = V.fromList $ crc32Table $ crcPoly cfg

-- | calculate CRC32 with extra configuration, table not calculated from poly.
crc32WithCfg :: Vector Word32 -> CRC32Cfg -> ByteString -> Word32
crc32WithCfg t cfg =
  xor (crcFinXor cfg)
    . applyWhen (crcRefOut cfg) ref32
    . crc32Unsafe t (crcRefIn cfg) (crcInit cfg)

-- | calculate CRC32 using an efficient lookup table and init value
crc32Unsafe :: Vector Word32 -> Bool -> Word32 -> ByteString -> Word32
crc32Unsafe t refIn = BS.foldl' go
  where
    go crc b = crc `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
      where
        pos = fromIntegral ((crc `xor` (fromIntegral b' `shiftL` 24)) `shiftR` 24) :: Word8
          where
            b' = applyWhen refIn ref8 b

-- | generate lookup table using polynomial
crc32Table :: Word32 -> [Word32]
crc32Table poly = calc <$> [0..255]
  where
    calc = appEndo (stimes (8 :: Int) $ Endo step) . (`shiftL` 24)
    step curByte = applyWhen (testBit curByte 31) (`xor` poly) $ curByte `shiftL` 1
