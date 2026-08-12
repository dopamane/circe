{-# LANGUAGE OverloadedStrings #-}

module Test.Circe.CRC32 (tests) where

import Codec.Circe.CRC32
import Codec.Circe.Cfg
import qualified Data.Vector.Storable as V
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CRC32"
  [ crc32UnsafeTest
  , crc32Test
  ]

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
