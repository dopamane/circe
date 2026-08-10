module Codec.Circe.Pretty
  ( w16
  ) where

import Data.Bits
import Data.Word
import Numeric

w16 :: Word16 -> String
w16 w = "0x" ++ w3 ++ w2 ++ w1 ++ w0
  where
    w3 = showHex (w `shiftR` 12) ""
    w2 = showHex (w `shiftR` 8 .&. 0xF) ""
    w1 = showHex (w `shiftR` 4 .&. 0xF) ""
    w0 = showHex (w .&. 0xF) ""
