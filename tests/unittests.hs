import Prelude

import Test.Tasty

import qualified Tests.Example.Project
import qualified Tests.LFSR.Fibonacci

main :: IO ()
main = defaultMain $ testGroup "."
  [
    Tests.Example.Project.accumTests,
    Tests.LFSR.Fibonacci.fibRNGTests
  ]
