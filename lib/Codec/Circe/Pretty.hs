-- | prettyprinting utilities
module Codec.Circe.Pretty
  ( w16
  , w32
  , table
  ) where

import Data.Bits
import Data.Word
import Numeric

-- | @0x01ab@
w16 :: Word16 -> String
w16 w = "0x" ++ w16' w

-- | pretty word16 without 0x prefix
w16' :: Word16 -> String
w16' w = w3 ++ w2 ++ w1 ++ w0
  where
    w3 = showHex (w `shiftR` 12) ""
    w2 = showHex (w `shiftR` 8 .&. 0xF) ""
    w1 = showHex (w `shiftR` 4 .&. 0xF) ""
    w0 = showHex (w .&. 0xF) ""

-- | @0x0123abcd@
w32 :: Word32 -> String
w32 w = "0x" ++ w16' h ++ w16' l
  where
    h = fromIntegral $ w `shiftR` 16
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
