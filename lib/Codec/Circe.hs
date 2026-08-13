-- | CRC utilities.
module Codec.Circe
  ( -- ** CRC8
    crc8
  , crc8WithTable
  , crc8Unsafe
  , crc8Table
  , CRC8Cfg
  , showCRC8Cfg
  , crc8Cfg
  , -- ** CRC16
    crc16
  , crc16WithTable
  , crc16Unsafe
  , crc16Table
  , CRC16Cfg
  , showCRC16Cfg
  , crc16CCITZero
  , crc16Modbus
  , -- ** CRC32
    crc32
  , crc32WithTable
  , crc32Unsafe
  , crc32Table
  , CRC32Cfg
  , showCRC32Cfg
  , crc32IEEE
  , -- ** CRC64
    crc64Table
  , CRC64Cfg
  , showCRC64Cfg
  , -- **** CFG
    CRCCfg(..)
  ) where

import Codec.Circe.Pretty
import Codec.Circe.Reflect
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Function
import Data.Word

-- | calculate CRC8 using lookup table generated from config
crc8 :: CRC8Cfg -> ByteString -> Word8
crc8 cfg = crc8WithTable t cfg
  where
    t = V.fromList $ crc8Table $ crcPoly cfg

-- | calculate CRC8 with precalculated poly table
crc8WithTable :: Vector Word8 -> CRC8Cfg -> ByteString -> Word8
crc8WithTable t cfg =
  xor (crcFinXor cfg)
    . applyWhen (crcRefOut cfg) ref8
    . crc8Unsafe t (crcRefIn cfg) (crcInit cfg)

-- | calculate CRC8 using efficient lookup table and init value
crc8Unsafe :: Vector Word8 -> Bool -> Word8 -> ByteString -> Word8
crc8Unsafe t refIn = BS.foldl' go
  where
    go crc b = t `V.unsafeIndex` fromIntegral pos
      where
        pos = crc `xor` applyWhen refIn ref8 b

-- | generate lookup table using polynomial
crc8Table :: Word8 -> [Word8]
crc8Table = crcTable 8

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
crc16Table = crcTable 16

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
crc32Table = crcTable 32

-- | generate lookup table using polynomial
crc64Table :: Word64 -> [Word64]
crc64Table = crcTable 64

crcTable :: (Bits a, Enum a, Num a) => Int -> a -> [a]
crcTable l poly = calc <$> [0..255]
  where
    calc = (!! 8) . iterate step . (`shiftL` (l - 8))
      where
        step curByte = applyWhen (testBit curByte $ l - 1) (`xor` poly) $ curByte `shiftL` 1

-- | CRC configuration parameters
data CRCCfg a = CRCCfg
  { crcPoly :: a
    -- ^ polynomial
  , crcInit :: a
    -- ^ initial value
  , crcRefIn :: Bool
    -- ^ reflect input
  , crcRefOut :: Bool
    -- ^ reflect output
  , crcFinXor :: a
    -- ^ final xor
  }
  deriving (Eq, Read, Show)

-- | display reflection config
showRefl :: (Bool, Bool) -> String
showRefl (ri, ro) = case (ri, ro) of
  (False, False) -> "NOREFL"
  (False, True)  -> "REFLOUT"
  (True,  False) -> "REFLIN"
  (True,  True)  -> "REFLINOUT"

-- | specialied CRC for 'Word8'
type CRC8Cfg = CRCCfg Word8

-- | display CRC8 config
showCRC8Cfg :: CRC8Cfg -> String
showCRC8Cfg (CRCCfg p i ri ro x) = unwords
  ["POLY", w8 p, "INIT", w8 i, showRefl (ri, ro), "XOR", w8 x]

crc8Cfg :: CRC8Cfg
crc8Cfg = CRCCfg 0x7 0x0 False False 0x0

-- | specialized CRC for 'Word16'
type CRC16Cfg = CRCCfg Word16

-- | display CRC16 config
showCRC16Cfg :: CRC16Cfg -> String
showCRC16Cfg (CRCCfg p i ri ro x) = unwords
  ["POLY", w16 p, "INIT", w16 i, showRefl (ri, ro), "XOR", w16 x]

-- | POLY 0x1021 INIT 0x0000 NOREFL NOXOR
crc16CCITZero :: CRC16Cfg
crc16CCITZero = CRCCfg 0x1021 0x0000 False False 0x0000

-- | POLY 0x8005 INIT 0XFFFF REFLINOUT NOXOR
crc16Modbus :: CRC16Cfg
crc16Modbus = CRCCfg 0x8005 0xFFFF True True 0x0000

-- | specialized CRC for 'Word32'
type CRC32Cfg = CRCCfg Word32

-- | display CRC32 config
showCRC32Cfg :: CRC32Cfg -> String
showCRC32Cfg (CRCCfg p i ri ro x) = unwords
  ["POLY", w32 p, "INIT", w32 i, showRefl (ri, ro), "XOR", w32 x]

-- | POLY 0x4C11DB7 INIT 0xFFFFFFFF REFLINOUT XOR 0xFFFFFFFF
crc32IEEE :: CRC32Cfg
crc32IEEE = CRCCfg 0x4C11DB7 0xFFFFFFFF True True 0xFFFFFFFF

-- | specialized CRC for 'Word64'
type CRC64Cfg = CRCCfg Word64

-- | display CRC64 config
showCRC64Cfg :: CRC64Cfg -> String
showCRC64Cfg (CRCCfg p i ri ro x) = unwords
  ["POLY", w64 p, "INIT", w64 i, showRefl (ri, ro), "XOR", w64 x]
