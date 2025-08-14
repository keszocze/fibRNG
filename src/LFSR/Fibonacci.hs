module LFSR.Fibonacci where

import Clash.Prelude


data FIBLFSR n = FIBLFSR {
  lfsr :: Unsigned n,
  taps :: Unsigned n
}

-- lfsr :: Unsigned n
