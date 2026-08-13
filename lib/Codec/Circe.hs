-- | CRC utilities.
module Codec.Circe
  ( -- ** CRC16
    crc16
  , crc16WithTable
  , crc16Unsafe
  , crc16Table
  , CRC16Cfg
  , crc16CCITZero
  , crc16Modbus
  , -- ** CRC32
    crc32
  , crc32WithTable
  , crc32Unsafe
  , crc32Table
  , CRC32Cfg
  , crc32IEEE
  ) where

import Codec.Circe.Cfg
import Codec.Circe.Reflect
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Function
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
    calc = (!! 8) . iterate step . (`shiftL` 8)
      where
        step curByte = applyWhen (testBit curByte 15) (`xor` poly) $ curByte `shiftL` 1

-- | calculate CRC32 using lookup table generated from config
crc32 :: CRC32Cfg -> ByteString -> Word32
crc32 cfg = crc32WithTable t cfg
  where
    t = V.fromList $ crc32Table $ crcPoly cfg

-- | calculate CRC32 with precalculated poly table.
crc32WithTable :: Vector Word32 -> CRC32Cfg -> ByteString -> Word32
crc32WithTable t cfg =
  xor (crcFinXor cfg)
    . applyWhen (crcRefOut cfg) ref32
    . crc32Unsafe t (crcRefIn cfg) (crcInit cfg)

-- | calculate CRC32 using an efficient lookup table and init value
crc32Unsafe :: Vector Word32 -> Bool -> Word32 -> ByteString -> Word32
crc32Unsafe t refIn = BS.foldl' go
  where
    go crc b = crc `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
      where
        pos = fromIntegral ((crc `xor` fromIntegral b' `shiftL` 24) `shiftR` 24) :: Word8
          where
            b' = applyWhen refIn ref8 b

-- | generate lookup table using polynomial
crc32Table :: Word32 -> [Word32]
crc32Table poly = calc <$> [0..255]
  where
    calc = (!! 8) . iterate step . (`shiftL` 24)
      where
        step curByte = applyWhen (testBit curByte 31) (`xor` poly) $ curByte `shiftL` 1
