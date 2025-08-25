module LFSR where

import Clash.Prelude


type LFSR n = Vec n Bit


lfsr' :: KnownNat n => (LFSR n -> Bit) -> LFSR n -> LFSR n
lfsr' f r = f r +>> r

lfsr :: forall dom n. (HiddenClockResetEnable dom, KnownNat n) => (LFSR n -> Bit) -> LFSR n -> Signal dom (LFSR n)
lfsr f i = reg
    where
        --reg :: Signal dom (LFSR n) -> Signal dom (LFSR n)
        reg = register i (fmap (lfsr' f) reg)
