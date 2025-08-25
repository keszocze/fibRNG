module Tests.LFSR.Fibonacci where

import Prelude

import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.Haskell.TH
import Tests.LFSR.Fibonacci.TH

import LFSR.Fibonacci.MaxLenTaps
import Test.Tasty.TH (testGroupGenerator)

-- prop_test :: H.Property
-- prop_test = H.property $ do
--   H.success

case_fibRNG2 = do
  let
    expected = C.repeat (1 :: C.Bit)

    result = C.sampleN @C.System 5 fibLFSR2
    resetValue = head result
    initValue = head $ tail result
    rest = tail $ tail result
    fstExpectedAgain = dropWhile (/= expected) rest


  resetValue @?= expected
  initValue @?= expected

  length fstExpectedAgain @?= 1



case_fibRNG3 = do
  let
    expected = C.repeat (1 :: C.Bit)

    result = C.sampleN @C.System 9 fibLFSR3
    resetValue = head result
    initValue = head $ tail result
    rest = tail $ tail result
    fstExpectedAgain = dropWhile (/= expected) rest


  resetValue @?= expected
  initValue @?= expected

  length fstExpectedAgain @?= 1


case_fibRNG4 = do
  let
    expected = C.repeat (1 :: C.Bit)

    result = C.sampleN @C.System 17 fibLFSR4
    resetValue = head result
    initValue = head $ tail result
    rest = tail $ tail result
    fstExpectedAgain = dropWhile (/= expected) rest


  resetValue @?= expected
  initValue @?= expected

  length fstExpectedAgain @?= 1


case_fibRNG5 = do
  let
    expected = C.repeat (1 :: C.Bit)

    result = C.sampleN @C.System 33 fibLFSR5
    resetValue = head result
    initValue = head $ tail result
    rest = tail $ tail result
    fstExpectedAgain = dropWhile (/= expected) rest


  resetValue @?= expected
  initValue @?= expected

  length fstExpectedAgain @?= 1





fibRNGTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain fibRNGTests
