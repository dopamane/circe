module Codec.Circe.Reflect (reflect, reflectTable, reflect') where

import Data.Bits
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V

-- | reflect using lookup table
reflect :: (Integral a, V.Storable a) => Vector a -> a -> a
reflect t a = t `V.unsafeIndex` fromIntegral a

-- | generate a reflected lookup table
reflectTable :: (Bounded a, Enum a, FiniteBits a, Num a) => [a]
reflectTable = reflect' <$> [0..maxBound]

-- | reflect bits
reflect' :: FiniteBits a => a -> a
reflect' a = getIor $ foldMap go [0 .. maxIdx]
  where
    maxIdx = finiteBitSize a - 1
    go idx | testBit a idx = Ior $ bit $ maxIdx - idx
           | otherwise     = mempty
