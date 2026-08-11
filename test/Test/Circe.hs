module Test.Circe (testCirce) where

import qualified Test.Circe.CRC16 as CRC16
import qualified Test.Circe.CRC32 as CRC32
import qualified Test.Circe.Reflect as Reflect
import Test.Tasty

testCirce :: IO ()
testCirce = defaultMain tests

tests :: TestTree
tests = testGroup "Test.Circe"
  [ CRC16.tests
  , CRC32.tests
  , Reflect.tests
  ]
