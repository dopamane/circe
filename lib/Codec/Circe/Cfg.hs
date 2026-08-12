-- | CRC configurations
module Codec.Circe.Cfg
  ( CRCCfg(..)
  , CRC16Cfg
  , crc16CCITZero
  , crc16Modbus
  , CRC32Cfg
  , crc32IEEE
  ) where

import Data.Word

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

-- | specialized CRC for 'Word16'
type CRC16Cfg = CRCCfg Word16
-- | specialized CRC for 'Word32'
type CRC32Cfg = CRCCfg Word32

-- | POLY 0x1021 INIT 0x0000 NOREFL NOXOR
crc16CCITZero :: CRC16Cfg
crc16CCITZero = CRCCfg 0x1021 0x0000 False False 0x0000

-- | POLY 0x8005 INIT 0XFFFF REFLINOUT NOXOR
crc16Modbus :: CRC16Cfg
crc16Modbus = CRCCfg 0x8005 0xFFFF True True 0x0000

-- | POLY 0x4C11DB7 INIT 0xFFFFFFFF REFLINOUT XOR 0xFFFFFFFF
crc32IEEE :: CRC32Cfg
crc32IEEE = CRCCfg 0x4C11DB7 0xFFFFFFFF True True 0xFFFFFFFF
