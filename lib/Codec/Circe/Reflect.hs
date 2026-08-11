module Codec.Circe.Reflect (reflect) where

import Data.Bits

reflect :: FiniteBits a => a -> a
reflect a = getIor $ foldMap go [0 .. maxIdx]
  where
    maxIdx = finiteBitSize a - 1
    go idx | testBit a idx = Ior $ bit $ maxIdx - idx
           | otherwise     = mempty
