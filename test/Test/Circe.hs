{-# LANGUAGE OverloadedStrings #-}
module Test.Circe (testCirce) where

import Codec.Circe
import qualified Data.Vector.Storable as V
import qualified Test.Circe.Reflect as Reflect
import Test.Tasty
import Test.Tasty.HUnit

testCirce :: IO ()
testCirce = defaultMain tests

tests :: TestTree
tests = testGroup "Test.Circe"
  [ testGroup "CRC16"
    [ crc16UnsafeTest
    , crc16Test
    ]
  , testGroup "CRC32"
    [ crc32UnsafeTest
    , crc32Test
    ]
  , Reflect.tests
  ]

crc16UnsafeTest :: TestTree
crc16UnsafeTest = testCase "crc16Unsafe" $
  crc16Unsafe (V.fromList t) False 0 "bosscoxwuzhere" @?= 0xbe42
  where
    t = crc16Table 0x1021

crc16Test :: TestTree
crc16Test = testCaseSteps "crc16" $ \step -> do
  step "modbus"
  crc16 crc16Modbus "hello world" @?= 0xddc7

crc32UnsafeTest :: TestTree
crc32UnsafeTest = testCase "crc32Unsafe" $
  crc32Unsafe (V.fromList t) False 0 "hello world" @?= 0x737af2ae
  where
    t = crc32Table 0x4c11db7

crc32Test :: TestTree
crc32Test = testCase "crc32" $ do
  crc32 crc32IEEE "bo$$ cox rulez" @?= 0x75ce60a3
  crc32 crc32IEEE "hello world"    @?= 0x0d4a1185
  crc32 crc32IEEE "YOLO DOLO"      @?= 0xa13ee2ed
