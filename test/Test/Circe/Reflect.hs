{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Circe.Reflect (tests) where

import Codec.Circe.Reflect
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Reflect"
  [ ref8Test
  , ref16Test
  , ref32Test
  ]

ref8Test :: TestTree
ref8Test = testCase "ref8" $
  ref8 0b1011_1110
   @?= 0b0111_1101

ref16Test :: TestTree
ref16Test = testCase "ref16" $
  ref16 0b1010_1111_0000_0011
    @?= 0b1100_0000_1111_0101

ref32Test :: TestTree
ref32Test = testCase "ref32" $
  ref32 0b1110_0111_0000_0000_1111_1111_1010_1010
    @?= 0b0101_0101_1111_1111_0000_0000_1110_0111
