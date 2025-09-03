module Tests.Fibonacci where

import Prelude

import Fibonacci
import Fibonacci.Taps

import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH ( testGroupGenerator )
import Test.Tasty.Hedgehog

import qualified Clash.Prelude as C
import Clash.Prelude (System, Bit, sampleN, listToVecTH)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- TODO alle Tests aus dem Verilog-Repo nachbauen (und darüber hinaus!)

import Test.Tasty.TH (testGroupGenerator)
case_fibRNG2 = do
  let
    expected = C.repeat (1 :: Bit)
    result = sampleN @System 5 $ fibonacciLFSR' $(listToVecTH $ getMaxLenTaps 2) (C.repeat (1 :: Bit))
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

    result = C.sampleN @C.System 9 $ fibonacciLFSR' $(listToVecTH $ getMaxLenTaps 3) (C.repeat (1 :: Bit))
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

    result = C.sampleN @C.System 17 $ fibonacciLFSR' $(listToVecTH $ getMaxLenTaps 4) (C.repeat (1 :: Bit))
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

    result = C.sampleN @C.System 33 $ fibonacciLFSR' $(listToVecTH $ getMaxLenTaps 5) (C.repeat (1 :: Bit))
    resetValue = head result
    initValue = head $ tail result
    rest = tail $ tail result
    fstExpectedAgain = dropWhile (/= expected) rest


  resetValue @?= expected
  initValue @?= expected

  length fstExpectedAgain @?= 1

case_fibRNG6 = do
  let
    expected = C.repeat (1 :: C.Bit)

    result = C.sampleN @C.System 65 $ fibonacciLFSR' $(listToVecTH $ getMaxLenTaps 6) (C.repeat (1 :: Bit))
    resetValue = head result
    initValue = head $ tail result
    rest = tail $ tail result
    fstExpectedAgain = dropWhile (/= expected) rest


  resetValue @?= expected
  initValue @?= expected

  length fstExpectedAgain @?= 1


case_fibRNG7 = do
  let
    expected = C.repeat (1 :: C.Bit)

    result = C.sampleN @C.System 129 $ fibonacciLFSR' $(listToVecTH $ getMaxLenTaps 7) (C.repeat (1 :: Bit))
    resetValue = head result
    initValue = head $ tail result
    rest = tail $ tail result
    fstExpectedAgain = dropWhile (/= expected) rest


  resetValue @?= expected
  initValue @?= expected

  length fstExpectedAgain @?= 1


case_fibRNG8 = do
  let
    expected = C.repeat (1 :: C.Bit)

    result = C.sampleN @C.System 257 $ fibonacciLFSR' $(listToVecTH $ getMaxLenTaps 8) (C.repeat (1 :: Bit))
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
