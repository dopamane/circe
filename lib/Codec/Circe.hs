-- | CRC utilities.
module Codec.Circe
  ( -- ** CRC8
    crc8, crc8WithTable, crc8Table
  , CRC8Cfg, showCRC8Cfg
  , crc8Cfg, crc88H2F, crc8CDMA2k, crc8DARC, crc8DVBS2, crc8EBU, crc8ICode
  , crc8ITU, crc8Maxim, crc8ROHC, crc8WCDMA
  , -- ** CRC16
    crc16, crc16WithTable, crc16Table
  , CRC16Cfg, showCRC16Cfg
  , crc16CCITZero, crc16Arc, crc16AugCCITT, crc16Buypass, crc16CCITTFalse
  , crc16CDMA2k, crc16DDS110, crc16DECTR, crc16DECTX, crc16DNP, crc16EN13757
  , crc16Genibus, crc16Maxim, crc16MCRF4XX, crc16Riello, crc16T10DIF
  , crc16Teledisk, crc16TMS37157, crc16USB, crc16CRCA, crc16Kermit
  , crc16Modbus, crc16X25, crc16XModem
  , -- ** CRC32
    crc32, crc32WithTable, crc32Table
  , CRC32Cfg, showCRC32Cfg
  , crc32IEEE, crc32BZIP2, crc32MPEG2, crc32POSIX
  , -- ** CRC64
    crc64, crc64WithTable, crc64Table
  , CRC64Cfg, showCRC64Cfg
  , crc64ECMA182, crc64GoISO, crc64WE, crc64XZ
  , -- ** CRCCfg
    CRCCfg(..)
  , -- ** Unsafe
    crc, crcWithTable, crcUnsafe, crcTable
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
crc8 = crc ref8
-- | calculate CRC16 using lookup table generated from config
crc16 :: CRC16Cfg -> ByteString -> Word16
crc16 = crc ref16
-- | calculate CRC32 using lookup table generated from config
crc32 :: CRC32Cfg -> ByteString -> Word32
crc32 = crc ref32
-- | calculate CRC64 using lookup table generated from config
crc64 :: CRC64Cfg -> ByteString -> Word64
crc64 = crc ref64

-- | calculate CRC8 with precalculated poly table
crc8WithTable :: Vector Word8 -> CRC8Cfg -> ByteString -> Word8
crc8WithTable = crcWithTable ref8
-- | calculate CRC16 with precalculated poly table
crc16WithTable :: Vector Word16 -> CRC16Cfg -> ByteString -> Word16
crc16WithTable = crcWithTable ref16
-- | calculate CRC32 with precalculated poly table
crc32WithTable :: Vector Word32 -> CRC32Cfg -> ByteString -> Word32
crc32WithTable = crcWithTable ref32
-- | calculate CRC64 with precalculated poly table
crc64WithTable :: Vector Word64 -> CRC64Cfg -> ByteString -> Word64
crc64WithTable = crcWithTable ref64

-- | generate CRC8 lookup table using polynomial
crc8Table :: Word8 -> [Word8]
crc8Table = crcTable
-- | generate CRC16 lookup table using polynomial
crc16Table :: Word16 -> [Word16]
crc16Table = crcTable
-- | generate CRC32 lookup table using polynomial
crc32Table :: Word32 -> [Word32]
crc32Table = crcTable
-- | generate CRC64 lookup table using polynomial
crc64Table :: Word64 -> [Word64]
crc64Table = crcTable

-- | calculate CRC using lookup table generated from config
crc :: (FiniteBits a, Integral a, Num a, V.Storable a) => (a -> a) -> CRCCfg a -> ByteString -> a
crc ref cfg = crcWithTable ref t cfg
  where
    t = V.fromList $ crcTable $ crcPoly cfg

-- | calculate CRC with reflection & precalculated poly table.
crcWithTable
  :: (FiniteBits a, Integral a, Num a, V.Storable a)
  => (a -> a)
  -> Vector a -> CRCCfg a -> ByteString -> a
crcWithTable ref t cfg =
  xor (crcFinXor cfg) . applyWhen (crcRefOut cfg) ref . crcUnsafe t (crcRefIn cfg) (crcInit cfg)

-- | calculate CRC using width, efficient lookup table, input reflection, and init value
crcUnsafe :: (FiniteBits a, Integral a, Num a, V.Storable a) => Vector a -> Bool -> a -> ByteString -> a
crcUnsafe t refIn i = BS.foldl' go i
  where
    go crc' b = crc' `shiftL` 8 `xor` t `V.unsafeIndex` fromIntegral pos
      where
        pos = fromIntegral ((crc' `xor` fromIntegral b' `shiftL` w') `shiftR` w') :: Word8
          where
            b' = applyWhen refIn ref8 b
            w' = finiteBitSize i - 8

-- | Generate CRC table using width and poly
crcTable :: (FiniteBits a, Enum a, Num a) => a -> [a]
crcTable poly = calc <$> [0..255]
  where
    l    = fromIntegral (finiteBitSize poly) :: Int
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

instance Functor CRCCfg where
  fmap f (CRCCfg p i ri ro x) = CRCCfg (f p) (f i) ri ro (f x)

showCRCCfg :: CRCCfg String -> String
showCRCCfg (CRCCfg p i ri ro x) = unwords ["POLY", p, "INIT", i, showRefl (ri, ro), "XOR", x]

-- | display reflection config
showRefl :: (Bool, Bool) -> String
showRefl (ri, ro) = case (ri, ro) of
  (False, False) -> "NOREFL"
  (False, True)  -> "REFLOUT"
  (True,  False) -> "REFLIN"
  (True,  True)  -> "REFLINOUT"

-- | specialized CRC for 'Word8'
type CRC8Cfg = CRCCfg Word8
-- | specialized CRC for 'Word16'
type CRC16Cfg = CRCCfg Word16
-- | specialized CRC for 'Word32'
type CRC32Cfg = CRCCfg Word32
-- | specialized CRC for 'Word64'
type CRC64Cfg = CRCCfg Word64

-- | display CRC8 config
showCRC8Cfg :: CRC8Cfg -> String
showCRC8Cfg = showCRCCfg . fmap w8
-- | display CRC16 config
showCRC16Cfg :: CRC16Cfg -> String
showCRC16Cfg = showCRCCfg . fmap w16
-- | display CRC32 config
showCRC32Cfg :: CRC32Cfg -> String
showCRC32Cfg = showCRCCfg . fmap w32
-- | display CRC64 config
showCRC64Cfg :: CRC64Cfg -> String
showCRC64Cfg = showCRCCfg . fmap w64

-- | POLY 0x7 INIT 0 NOREFL NOXOR
crc8Cfg :: CRC8Cfg
crc8Cfg = CRCCfg 0x7 0x0 False False 0x0
-- | POLY 0x2F INIT 0xFF NOREFL XOR 0xFF
crc88H2F :: CRC8Cfg
crc88H2F = CRCCfg 0x2f 0xff False False 0xff
-- | POLY 0x9b INIT 0xFF NOREFL NOXOR
crc8CDMA2k :: CRC8Cfg
crc8CDMA2k = CRCCfg 0x9b 0xFF False False 0
-- | POLY 0x39 INIT 0 REFLINOUT NOXOR
crc8DARC :: CRC8Cfg
crc8DARC = CRCCfg 0x39 0 True True 0
-- | POLY 0xD5 INIT 0 NOREFL XOR 0
crc8DVBS2 :: CRC8Cfg
crc8DVBS2 = CRCCfg 0xD5 0 False False 0
-- | POLY 0x1D INIT 0xFF REFLINOUT XOR 0
crc8EBU :: CRC8Cfg
crc8EBU = CRCCfg 0x1D 0xFF True True 0
-- | POLY 0x1D INIT 0xFD NOREFL XOR 0
crc8ICode :: CRC8Cfg
crc8ICode = CRCCfg 0x1D 0xFD False False 0
-- | POLY 0x07 INIT 0x00 NOREFL XOR 0x55
crc8ITU :: CRC8Cfg
crc8ITU = CRCCfg 0x07 0x00 False False 0x55
-- | POLY 0x31 INIT 0 REFLINOUT XOR 0
crc8Maxim :: CRC8Cfg
crc8Maxim = CRCCfg 0x31 0 True True 0
-- | POLY 0x07 INIT 0xFF REFLINOUT XOR 0
crc8ROHC :: CRC8Cfg
crc8ROHC = CRCCfg 0x07 0xFF True True 0
-- | POLY 0x9b INIT 0 REFLINOUT NOXOR
crc8WCDMA :: CRC8Cfg
crc8WCDMA = CRCCfg 0x9b 0 True True 0

-- | POLY 0x1021 INIT 0x0000 NOREFL NOXOR
crc16CCITZero :: CRC16Cfg
crc16CCITZero = CRCCfg 0x1021 0x0000 False False 0x0000
-- | POLY 0x8005 INIT 0 REFLINOUT XOR 0
crc16Arc :: CRC16Cfg
crc16Arc = CRCCfg 0x8005 0 True True 0
-- | POLY 0x1021 INIT 0x1D0F NOREFL XOR 0
crc16AugCCITT :: CRC16Cfg
crc16AugCCITT = CRCCfg 0x1021 0x1D0F False False 0
-- | POLY 0x8005 INIT 0 NOREFL XOR 0
crc16Buypass :: CRC16Cfg
crc16Buypass = CRCCfg 0x8005 0 False False 0
-- | POLY 0x1021 INIT 0xFFFF NOREFL XOR 0
crc16CCITTFalse :: CRC16Cfg
crc16CCITTFalse = CRCCfg 0x1021 0xFFFF False False 0
-- | POLY 0xC867 INIT 0xFFFF NOREFL XOR 0
crc16CDMA2k :: CRC16Cfg
crc16CDMA2k = CRCCfg 0xC867 0xFFFF False False 0
-- | POLY 0x8005 INIT 0x800D NOREFL XOR 0
crc16DDS110 :: CRC16Cfg
crc16DDS110 = CRCCfg 0x8005 0x800D False False 0
-- | POLY 0x0589 INIT 0 NOREFL XOR 1
crc16DECTR :: CRC16Cfg
crc16DECTR = CRCCfg 0x0589 0 False False 1
-- | POLY 0x0589 INIT 0 NOREFL XOR 0
crc16DECTX :: CRC16Cfg
crc16DECTX = CRCCfg 0x0589 0 False False 0
-- | POLY 0x3D65 INIT 0 REFLINOUT XOR 0xFFFF
crc16DNP :: CRC16Cfg
crc16DNP = CRCCfg 0x3D65 0 True True 0xFFFF
-- | POLY 0x3D65 INIT 0 NOREFL XOR 0xFFFF
crc16EN13757 :: CRC16Cfg
crc16EN13757 = CRCCfg 0x3D65 0 False False 0xFFFF
-- | POLY 0x1021 INIT 0xFFFF NOREFL XOR 0xFFFF
crc16Genibus :: CRC16Cfg
crc16Genibus = CRCCfg 0x1021 0xFFFF False False 0xFFFF
-- | POLY 0x8005 INIT 0 REFLINOUT XOR 0xFFFF
crc16Maxim :: CRC16Cfg
crc16Maxim = CRCCfg 0x8005 0 True True 0xFFFF
-- | POLY 0x1021 INIT 0xFFFF REFLINOUT XOR 0
crc16MCRF4XX :: CRC16Cfg
crc16MCRF4XX = CRCCfg 0x1021 0xFFFF True True 0
-- | POLY 0x1021 INIT 0xB2AA REFLINOUT XOR 0
crc16Riello :: CRC16Cfg
crc16Riello = CRCCfg 0x1021 0xB2AA True True 0
-- | POLY 0x8BB7 INIT 0 NOREFL XOR 0
crc16T10DIF :: CRC16Cfg
crc16T10DIF = CRCCfg 0x8BB7 0 False False 0
-- | POLY 0xA097 INIT 0 NOREFL XOR 0
crc16Teledisk :: CRC16Cfg
crc16Teledisk = CRCCfg 0xA097 0 False False 0
-- | POLY 0x1021 INIT 0x89EC REFLINOUT XOR 0
crc16TMS37157 :: CRC16Cfg
crc16TMS37157 = CRCCfg 0x1021 0x89EC True True 0
-- | POLY 0x8005 INIT 0xFFFF REFLINOUT XOR 0xFFFF
crc16USB :: CRC16Cfg
crc16USB = CRCCfg 0x8005 0xFFFF True True 0xFFFF
-- | POLY 0x1021 INIT 0xC6C6 REFLINOUT XOR 0
crc16CRCA :: CRC16Cfg
crc16CRCA = CRCCfg 0x1021 0xC6C6 True True 0
-- | POLY 0x1021 INIT 0 REFLINOUT XOR 0
crc16Kermit :: CRC16Cfg
crc16Kermit = CRCCfg 0x1021 0 True True 0
-- | POLY 0x8005 INIT 0XFFFF REFLINOUT NOXOR
crc16Modbus :: CRC16Cfg
crc16Modbus = CRCCfg 0x8005 0xFFFF True True 0x0000
-- | POLY 0x1021 INIT 0xFFFF REFLINOUT XOR 0xFFFF
crc16X25 :: CRC16Cfg
crc16X25 = CRCCfg 0x1021 0xFFFF True True 0xFFFF
-- | POLY 0x1021 INIT 0 NOREFL XOR 0
crc16XModem :: CRC16Cfg
crc16XModem = CRCCfg 0x1021 0 False False 0

-- | POLY 0x4C11DB7 INIT 0xFFFFFFFF REFLINOUT XOR 0xFFFFFFFF
crc32IEEE :: CRC32Cfg
crc32IEEE = CRCCfg 0x4C11DB7 0xFFFFFFFF True True 0xFFFFFFFF
-- | POLY 0x4C11DB7 INIT 0xFFFFFFFF NOREFL XOR 0xFFFFFFFF
crc32BZIP2 :: CRC32Cfg
crc32BZIP2 = CRCCfg 0x4C11DB7 0xFFFFFFFF False False 0xFFFFFFFF
-- | POLY 0x4C11DB7 INIT 0xFFFFFFFF NOREFL XOR 0
crc32MPEG2 :: CRC32Cfg
crc32MPEG2 = CRCCfg 0x4C11DB7 0xFFFFFFFF False False 0
-- | POLY 0x4C11DB7 INIT 0 NOREFL XOR 0xFFFFFFFF
crc32POSIX :: CRC32Cfg
crc32POSIX = CRCCfg 0x4C11DB7 0 False False 0xFFFFFFFF

-- | POLY 0x42F0E1EBA9EA3693 INIT 0 NOREFL XOR 0
crc64ECMA182 :: CRC64Cfg
crc64ECMA182 = CRCCfg 0x42F0E1EBA9EA3693 0 False False 0
-- | POLY 0x000000000000001B INIT 0xFFFFFFFFFFFFFFFF REFLINOUT XOR 0xFFFFFFFFFFFFFFFF
crc64GoISO :: CRC64Cfg
crc64GoISO = CRCCfg 0x000000000000001B 0xFFFFFFFFFFFFFFFF True True 0xFFFFFFFFFFFFFFFF
-- | POLY 0x42F0E1EBA9EA3693 INIT 0xFFFFFFFFFFFFFFFF NOREFL XOR 0xFFFFFFFFFFFFFFFF
crc64WE :: CRC64Cfg
crc64WE = CRCCfg 0x42F0E1EBA9EA3693 0xFFFFFFFFFFFFFFFF False False 0xFFFFFFFFFFFFFFFF
-- | POLY 0x42F0E1EBA9EA3693 INIT 0xFFFFFFFFFFFFFFFF REFLINOUT XOR 0xFFFFFFFFFFFFFFFF
crc64XZ :: CRC64Cfg
crc64XZ = CRCCfg 0x42F0E1EBA9EA3693 0xFFFFFFFFFFFFFFFF True True 0xFFFFFFFFFFFFFFFF
