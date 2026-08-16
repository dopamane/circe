-- | Internal CRC functions. See "Codec.Circe" for the stable API.
module Codec.Circe.Internal
  ( -- ** CRC
    CRCCfg(..), showCRCCfg
  , crcInternal, crcWithTable, crcUnsafe, crcStep, crcIdx, crcTable
  , -- ** Prettyprinting
    w8, w16, w32, w64, table
  , -- ** Reflection
    ref64, ref32, ref16, ref8
  ) where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Function
import Data.Word
import Numeric

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

-- | pretty cfg
showCRCCfg :: CRCCfg String -> String
showCRCCfg (CRCCfg p i ri ro x) = unwords ["POLY", p, "INIT", i, showRefl (ri, ro), "XOR", x]

-- | display reflection config
showRefl :: (Bool, Bool) -> String
showRefl (ri, ro) = case (ri, ro) of
  (False, False) -> "NOREFL"
  (False, True)  -> "REFLOUT"
  (True,  False) -> "REFLIN"
  (True,  True)  -> "REFLINOUT"

-- | calculate CRC using lookup table generated from config
crcInternal :: (FiniteBits a, Integral a, Num a, V.Storable a) => (a -> a) -> CRCCfg a -> ByteString -> a
crcInternal ref cfg = crcWithTable ref t cfg
  where
    t = V.fromList $ crcTable $ crcPoly cfg

-- | calculate CRC with reflection & precalculated poly table.
crcWithTable
  :: (FiniteBits a, Integral a, Num a, V.Storable a)
  => (a -> a) -> Vector a -> CRCCfg a -> ByteString -> a
crcWithTable ref t cfg =
  xor (crcFinXor cfg) . applyWhen (crcRefOut cfg) ref . crcUnsafe t (crcRefIn cfg) (crcInit cfg)

-- | calculate CRC using width, efficient lookup table, input reflection, and init value
crcUnsafe :: (FiniteBits a, Integral a, Num a, V.Storable a) => Vector a -> Bool -> a -> ByteString -> a
crcUnsafe t refIn = BS.foldl' $ \crc -> crcStep t crc . applyWhen refIn ref8

-- | single crc iteration with table and accumulator
crcStep :: (FiniteBits a, Integral a, Num a, V.Storable a) => Vector a -> a -> Word8 -> a
crcStep t crc b = crc `shiftL` 8 `xor` t `V.unsafeIndex` crcIdx crc b

-- | calculate lookup table index
crcIdx :: (FiniteBits a, Integral a, Num a) => a -> Word8 -> Int
crcIdx crc b' = fromIntegral ((crc `xor` fromIntegral b' `shiftL` w') `shiftR` w')
  where
    w' = finiteBitSize crc - 8

-- | Generate CRC table using width and poly
crcTable :: (FiniteBits a, Enum a, Num a) => a -> [a]
crcTable poly = calc <$> [0..255]
  where
    l    = fromIntegral (finiteBitSize poly) :: Int
    calc = (!! 8) . iterate step . (`shiftL` (l - 8))
      where
        step curByte = applyWhen (testBit curByte $ l - 1) (xor poly) $ curByte `shiftL` 1

-- | @0x0a@
w8 :: Word8 -> String
w8 w = "0x" ++ w8' w

-- | pretty word8 without 0x prefix
w8' :: Word8 -> String
w8' w = showHex (w `shiftR` 4) "" ++ showHex (w .&. 0xF) ""

-- | @0x01ab@
w16 :: Word16 -> String
w16 w = "0x" ++ w16' w

-- | pretty word16 without 0x prefix
w16' :: Word16 -> String
w16' w = w8' h ++ w8' l
  where
    h = fromIntegral $ w `shiftR` 8
    l = fromIntegral w

-- | @0x0123abcd@
w32 :: Word32 -> String
w32 w = "0x" ++ w32' w

-- | pretty word32 without 0x prefix
w32' :: Word32 -> String
w32' w = w16' h ++ w16' l
  where
    h = fromIntegral $ w `shiftR` 16
    l = fromIntegral w

-- | @0x01234567abcdefff@
w64 :: Word64 -> String
w64 w = "0x" ++ w32' h ++ w32' l
  where
    h = fromIntegral $ w `shiftR` 32
    l = fromIntegral w

-- | prettyprint 8 cols across
table :: [String] -> String
table = unlines . map unwords . chunks 8

-- | chunk a list
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = ys : chunks n zs
  where
    (ys, zs) = splitAt n xs

-- | Reflect 'Word64'
ref64 :: Word64 -> Word64
ref64 = s1 . s2 . s4 . s8 . s16 . s32
  where
    s1  n = ((n .&. 0xAAAAAAAAAAAAAAAA) `shiftR` 1)  .|. ((n .&. 0x5555555555555555) `shiftL` 1)
    s2  n = ((n .&. 0xCCCCCCCCCCCCCCCC) `shiftR` 2)  .|. ((n .&. 0x3333333333333333) `shiftL` 2)
    s4  n = ((n .&. 0xF0F0F0F0F0F0F0F0) `shiftR` 4)  .|. ((n .&. 0x0F0F0F0F0F0F0F0F) `shiftL` 4)
    s8  n = ((n .&. 0xFF00FF00FF00FF00) `shiftR` 8)  .|. ((n .&. 0x00FF00FF00FF00FF) `shiftL` 8)
    s16 n = ((n .&. 0xFFFF0000FFFF0000) `shiftR` 16) .|. ((n .&. 0x0000FFFF0000FFFF) `shiftL` 16)
    s32 n = (n `shiftR` 32) .|. (n `shiftL` 32)

-- | Reflect 'Word32'
ref32 :: Word32 -> Word32
ref32 = s1 . s2 . s4 . s8 . s16
  where
    s1  n = ((n .&. 0xAAAAAAAA) `shiftR` 1) .|. ((n .&. 0x55555555) `shiftL` 1)
    s2  n = ((n .&. 0xCCCCCCCC) `shiftR` 2) .|. ((n .&. 0x33333333) `shiftL` 2)
    s4  n = ((n .&. 0xF0F0F0F0) `shiftR` 4) .|. ((n .&. 0x0F0F0F0F) `shiftL` 4)
    s8  n = ((n .&. 0xFF00FF00) `shiftR` 8) .|. ((n .&. 0x00FF00FF) `shiftL` 8)
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
ref8 = V.unsafeIndex ref8Table . fromIntegral

ref8Table :: Vector Word8
ref8Table = V.fromList
  [ 0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0
  , 0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0
  , 0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8
  , 0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8
  , 0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4
  , 0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4
  , 0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec
  , 0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc
  , 0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2
  , 0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2
  , 0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea
  , 0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa
  , 0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6
  , 0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6
  , 0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee
  , 0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe
  , 0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1
  , 0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1
  , 0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9
  , 0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9
  , 0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5
  , 0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5
  , 0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed
  , 0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd
  , 0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3
  , 0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3
  , 0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb
  , 0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb
  , 0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7
  , 0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7
  , 0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef
  , 0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
  ]
