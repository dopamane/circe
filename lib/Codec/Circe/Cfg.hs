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

data CRCCfg a = CRCCfg
  { crcPoly :: a
  , crcInit :: a
  , crcRefIn :: Bool
  , crcRefOut :: Bool
  , crcFinXor :: a
  }
  deriving (Eq, Read, Show)

type CRC16Cfg = CRCCfg Word16
type CRC32Cfg = CRCCfg Word32

crc16CCITZero :: CRC16Cfg
crc16CCITZero = CRCCfg 0x1021 0x0000 False False 0x0000

crc16Modbus :: CRC16Cfg
crc16Modbus = CRCCfg 0x8005 0xFFFF True True 0x0000

crc32IEEE :: CRC32Cfg
crc32IEEE = CRCCfg 0x4C11DB7 0xFFFFFFFF True True 0xFFFFFFFF
