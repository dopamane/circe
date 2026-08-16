{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main (main) where

import Codec.Circe
import Codec.Circe.Internal
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.Circe"
  [crc8Test, crc16Test, crc32Test, crc64Test, prettyTest, reflectTest]

crc8Test :: TestTree
crc8Test = testCaseSteps "crc8" $ \step -> do
  step "crc8"
  crc8 crc8Cfg "alpha and omega" @?= 0x6f
  crc8 crc8Cfg "black and yellow" @?= 0x67
  crc8 crc8Cfg "w2wgaBRJWx0aU0j9V0bUZqLLNtYGIJ" @?= 0x42
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

crc16Test :: TestTree
crc16Test = testCaseSteps "crc16" $ \step -> do
  step "ccitt-zero"
  crc16 crc16CCITZero "bosscoxwuzhere" @?= 0xbe42
  crc16 crc16CCITZero "LRwI5WKpTxoS23UKsibhwN0qy55rj3" @?= 0x53ef
  step "modbus"
  crc16 crc16Modbus "hello world" @?= 0xddc7
  step "X25"
  crc16 crc16X25 "city of angels" @?= 0x5386

crc32Test :: TestTree
crc32Test = testCase "crc32" $ do
  crc32 (CRCCfg 0x4c11db7 0 False False 0) "hello world" @?= 0x737af2ae
  crc32 crc32IEEE "bo$$ cox rulez" @?= 0x75ce60a3
  crc32 crc32IEEE "hello world" @?= 0x0d4a1185
  crc32 crc32IEEE "YOLO DOLO" @?= 0xa13ee2ed
  crc32 crc32IEEE "A3FCx@Tgv(fS1yaGE=QG%T&:ffG2!q" @?= 0xeef35f36

crc64Test :: TestTree
crc64Test = testCaseSteps "crc64" $ \step -> do
  step "ECMA182"
  crc64 crc64ECMA182 "fast cars" @?= 0x5c991e3b22f9bd5f
  crc64 crc64ECMA182 "f.Mc.4B}fu:[e#+_beaqP!wF}[/avf" @?= 0xb102fcf42a9b6f57
  step "XZ"
  crc64 crc64XZ "goodbye" @?= 0x8F627A49FD449B48

prettyTest :: TestTree
prettyTest = testCase "pretty" $ do
  w8  0x00 @?= "0x00"
  w8  0x1f @?= "0x1f"
  w16 0x01a9 @?= "0x01a9"
  w32 0xbeefdead @?= "0xbeefdead"
  w64 0xdead1334cafeface @?= "0xdead1334cafeface"

reflectTest :: TestTree
reflectTest = testCaseSteps "reflect" $ \step -> do
  step "ref8"
  ref8 0b1011_1110 @?= 0b0111_1101
  ref8 0b1111_0000 @?= 0b0000_1111
  step "ref16"
  ref16 0b1010_1111_0000_0011 @?= 0b1100_0000_1111_0101
  ref16 0b1111_0110_0101_1100 @?= 0b0011_1010_0110_1111
  step "ref32"
  ref32 0b1110_0111_0000_0000_1111_1111_1010_1010
    @?= 0b0101_0101_1111_1111_0000_0000_1110_0111
  ref32 0b1000_0010_0111_1000_0001_0101_1111_0000
    @?= 0b0000_1111_1010_1000_0001_1110_0100_0001
