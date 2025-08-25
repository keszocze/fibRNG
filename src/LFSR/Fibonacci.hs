module LFSR.Fibonacci where

import Clash.Prelude
import qualified Control.Monad.Trans.State.Strict as S
import LFSR

type Taps n = Vec n Bit

fibLFSR' :: (KnownNat n) => Taps (n + 1) -> LFSR (n + 1) -> LFSR (n + 1)
fibLFSR' taps reg = reg'
  where
    selection = zipWith (.&.) reg taps
    newBit = fold xor selection
    reg' = newBit +>> reg

fibLFSR :: (HiddenClockResetEnable dom, KnownNat n) => Taps (n + 1) -> LFSR (n + 1) -> Signal dom (LFSR (n + 1))
fibLFSR taps i = reg
  where
    reg = register i (fmap (fibLFSR' taps) reg)

fibLFSRGen' :: Taps (n + 1) -> LFSR (n + 1) -> Bit
fibLFSRGen' taps reg = newBit
  where
    selection = zipWith (.&.) reg taps
    newBit = fold xor selection

fibLFSRGen :: (HiddenClockResetEnable dom, KnownNat n) => Taps (n + 1) -> LFSR (n + 1) -> Signal dom (LFSR (n + 1))
fibLFSRGen taps = lfsr (fibLFSRGen' taps)

{-
Adressen für die LSFR Werte
0000
0001
0010
0011

Adressen für die Taps Werte
0100
0101
0110
0111

Adresse für die Konfiguration
1111
-}

-- fibThingie ::
--   (KnownNat nWords, KnownNat wordSize, KnownNat addrWidth) =>
--   BitVector wordSize ->
--   BitVector addrWidth ->
--   Maybe (BitVector wordSize) ->
-- (BitVector wordSize, BitVector wordSize)




{-
Muss zwischen drei Dingen unterscheiden:
Interface width/ "adress"

-}


-- -- TODO nWordsExp -> addrWidth!
-- getSubWord :: forall wordSize wordIdxWidth. (KnownNat wordSize, KnownNat wordIdxWidth) => FibState' (wordSize * addrWidth)-> BitVector addrWidth -> BitVector wordSize
-- getSubWord (FibState' { reg, taps }) addr = extractWord from wordIdx
--   where
--     from = if testBit addr (natToNum @addrWidth) then taps else reg
--     wordIdx = undefined -- TODO this isnt correct any more: resize addr :: BitVector addrWidth
--     extractWord = undefined

data FibMode = Stopped | Running | Stepwise

data FibState' w = FibState'
  { reg :: LFSR w,
    taps :: Taps w,
    mode :: FibMode
  }

-- TODO herausfinden, ob es einen expliziten read request gibt, ob die adresse immer anliegt und ich dann steps über einen write realisieren muss

type FibState wordSize nWords a = S.State (FibState' (wordSize * nWords)) a

data AddressType = LFSR | Taps

data Address n = Address AddressType (BitVector n)

-- Address ist ein unnötiges Level an Indirektion?
data FibCmd wordSize addrWidth = Read (Address addrWidth) | Write (Address addrWidth) (BitVector wordSize)

readFib :: AddressType -> FibState' w -> BitVector w'
readFib = undefined

fibThingie :: forall wordSize addrWidth nWords.
  ( KnownNat wordSize,
    KnownNat addrWidth,
    KnownNat nWords,
    nWords ~ 2 ^ addrWidth,
    CmpNat (wordSize * nWords + 1) wordSize ~ GT
  ) =>
  FibCmd wordSize addrWidth ->
  FibState wordSize nWords (BitVector wordSize)
fibThingie (Read (Address addrType addr)) = do
  vec <- getVector addrType
  -- put the desired bits at the correct position
  let
    v = rotateLeft vec addr
    w = withSNat @wordSize (\n -> select d0 d1 n v)
  -- the actuall address within the LSFR/Taps Vec
  return $ bitCoerce w

fibThingie (Write add val) = do
  return 0


-- getVector :: forall wordSize nWords. (KnownNat wordSize, KnownNat nWords) => AddressType -> FibState wordSize nWords (Vec (wordSize * nWords) Bit)
getVector LFSR = S.gets reg
getVector Taps = S.gets taps


