-- | prettyprinting utilities
module Codec.Circe.Pretty
  ( w8
  , w16
  , w32
  , w64
  , table
  ) where

import Data.Bits
import Data.Word
import Numeric

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
