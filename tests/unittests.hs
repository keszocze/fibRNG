import Prelude

import Test.Tasty

import qualified Tests.Fibonacci

main :: IO ()
main = defaultMain Tests.Fibonacci.fibRNGTests

