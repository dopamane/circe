{-# LANGUAGE OverloadedStrings #-}

module Test.Circe.CRC16 (tests) where

import Codec.Circe.CRC16
import Codec.Circe.Cfg
--import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CRC16"
  [ crc16UnsafeTest
  , crc16Test
  ]

crc16UnsafeTest :: TestTree
crc16UnsafeTest = testCase "crc16Unsafe" $
  crc16Unsafe (V.fromList t) 0 "bosscoxwuzhere" @?= 0xbe42
  where
    t = crc16Table 0x1021

crc16Test :: TestTree
crc16Test = testCaseSteps "crc16" $ \step -> do
  step "modbus"
  crc16 crc16Modbus "hello world" @?= 0xddc7
