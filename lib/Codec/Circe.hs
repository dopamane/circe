-- | CRC high-level utilities.
-- See "Codec.Circe.CRC16" and "Codec.Circe.CRC32" for low-level functions.
module Codec.Circe
  ( -- ** CRC
    crc16
  , crc32
  , -- ** CONFIG
    CRCCfg(..)
  , -- **** CRC16
    CRC16Cfg
  , crc16CCITZero
  , crc16Modbus
  , -- **** CRC32
    CRC32Cfg
  , crc32IEEE
  ) where

import Codec.Circe.CRC16
import Codec.Circe.CRC32
import Codec.Circe.Cfg
