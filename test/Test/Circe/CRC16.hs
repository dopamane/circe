{-# LANGUAGE OverloadedStrings #-}

module Test.Circe.CRC16 (tests) where

import Codec.Circe.CRC16
--import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CRC16"
  [ basicTest
  ]

basicTest :: TestTree
basicTest = testCase "crc16Unsafe" $
  crc16Unsafe (V.fromList t) 0 "bosscoxwuzhere" @?= 0xbe42
  where
    t = crc16Table 0x1021
