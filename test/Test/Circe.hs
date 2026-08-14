{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Test.Circe (testCirce) where

import Codec.Circe
import Codec.Circe.Reflect
import qualified Data.Vector.Storable as V
import Test.Tasty
import Test.Tasty.HUnit

testCirce :: IO ()
testCirce = defaultMain tests

tests :: TestTree
tests = testGroup "Test.Circe"
  [ testGroup "CRC8"
    [ crc8Test
    ]
  , testGroup "CRC16"
    [ crc16UnsafeTest
    , crc16Test
    ]
  , testGroup "CRC32"
    [ crc32UnsafeTest
    , crc32Test
    ]
  , testGroup "CRC64"
    [ crc64Test
    ]
  , testGroup "Reflect"
    [ ref8Test
    , ref16Test
    , ref32Test
    ]
  ]

crc8Test :: TestTree
crc8Test = testCaseSteps "crc8" $ \step -> do
  step "crc8"
  crc8 crc8Cfg "alpha and omega" @?= 0x6f
  crc8 crc8Cfg "black and yellow" @?= 0x67
  step "crc88H2F"
  crc8 crc88H2F "yamborghini high" @?= 0x02
  step "crc8CDMA2k"
  crc8 crc8CDMA2k "asdfgh" @?= 0x79
  step "crc8DARC"
  crc8 crc8DARC "i am god" @?= 0x5a
  step "crc8ITU"
  crc8 crc8ITU "OMG itz GODZILLA!" @?= 0xf9
  step "crc8WCDMA"
  crc8 crc8WCDMA "city of angels" @?= 0x5d

crc16UnsafeTest :: TestTree
crc16UnsafeTest = testCase "crc16Unsafe" $
  crcUnsafe (V.fromList t) False 0 "bosscoxwuzhere" @?= 0xbe42
  where
    t = crc16Table 0x1021

crc16Test :: TestTree
crc16Test = testCaseSteps "crc16" $ \step -> do
  step "modbus"
  crc16 crc16Modbus "hello world" @?= 0xddc7
  step "X25"
  crc16 crc16X25 "city of angels" @?= 0x5386

crc32UnsafeTest :: TestTree
crc32UnsafeTest = testCase "crc32Unsafe" $
  crcUnsafe (V.fromList t) False 0 "hello world" @?= 0x737af2ae
  where
    t = crc32Table 0x4c11db7

crc32Test :: TestTree
crc32Test = testCase "crc32" $ do
  crc32 crc32IEEE "bo$$ cox rulez" @?= 0x75ce60a3
  crc32 crc32IEEE "hello world"    @?= 0x0d4a1185
  crc32 crc32IEEE "YOLO DOLO"      @?= 0xa13ee2ed

crc64Test :: TestTree
crc64Test = testCaseSteps "crc64" $ \step -> do
  step "ECMA182"
  crc64 crc64ECMA182 "fast cars" @?= 0x5c991e3b22f9bd5f
  step "XZ"
  crc64 crc64XZ "goodbye" @?= 0x8F627A49FD449B48

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
