{-# LANGUAGE OverloadedStrings #-}

module Test.Circe.CRC32 (tests) where

import Codec.Circe.CRC32
import qualified Data.Vector.Storable as V
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CRC32"
  [ crc32UnsafeTest
  ]

crc32UnsafeTest :: TestTree
crc32UnsafeTest = testCase "crc32Unsafe" $
  crc32Unsafe (V.fromList t) 0 "hello world" @?= 0x737af2ae
  where
    t = crc32Table 0x4c11db7
