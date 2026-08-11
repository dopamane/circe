{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Circe.Reflect (tests) where

import Codec.Circe.Reflect
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Reflect"
  [ word8Test
  , word16Test
  , word32Test
  ]

word8Test :: TestTree
word8Test = testCase "word8" $
  reflect' 0b1011_1110 @?= 0b0111_1101
  where
    reflect' = reflect :: Word8 -> Word8

word16Test :: TestTree
word16Test = testCase "word16" $
  reflect' 0b1010_1111_0000_0011 @?= 0b1100_0000_1111_0101
  where
    reflect' = reflect :: Word16 -> Word16

word32Test :: TestTree
word32Test = testCase "word32" $
  reflect' 0b1110_0111_0000_0000_1111_1111_1010_1010
       @?= 0b0101_0101_1111_1111_0000_0000_1110_0111
  where
    reflect' = reflect :: Word32 -> Word32
