module Test.Circe (testCirce) where

import qualified Test.Circe.CRC16 as CRC16
import Test.Tasty

testCirce :: IO ()
testCirce = defaultMain tests

tests :: TestTree
tests = testGroup "Test.Circe"
  [ CRC16.tests
  ]
