module LFSR.Fibonacci where

import Clash.Prelude
import qualified Control.Monad.Trans.State.Strict as S
import Debug.Trace
import LFSR
import LFSR.Fibonacci.TH

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

data FibMode = Stopped | Running | Stepwise deriving (Show, Generic, NFDataX)

data FibState' w = FibState'
  { reg :: LFSR w,
    taps :: Taps w,
    mode :: FibMode
  }
  deriving (Show, Generic, NFDataX)

-- TODO herausfinden, ob es einen expliziten read request gibt, ob die adresse immer anliegt und ich dann steps über einen write realisieren muss

type FibState wordSize nWords a = S.State (FibState' (wordSize * nWords)) a

data AddressType = LFSR | Taps deriving (Eq, Show, Generic, NFDataX)

data Address n = Address AddressType (Unsigned n) deriving (Show, Generic, NFDataX)

-- Address ist ein unnötiges Level an Indirektion?
data FibCmd wordSize addrWidth = Read (Address addrWidth) | Write (Address addrWidth) (BitVector wordSize)
  deriving (Show, Generic, NFDataX)

readVec :: AddressType -> FibState' w -> Vec w Bit
readVec addrType s = if addrType == LFSR then reg s else taps s

a = (1 :: Bit) :> 0 :> 1 :> 1 :> 0 :> 0 :> 0 :> 1 :> Nil

readWord ::
  forall wordSize addrWidth w.
  ( KnownNat wordSize,
    KnownNat addrWidth,
    KnownNat w,
    CmpNat (w + 1) wordSize ~ GT
  ) =>
  Address addrWidth ->
  FibState' w ->
  BitVector wordSize
readWord (Address addrType addr) s = word
  where
    vec = readVec addrType s
    word = readWord' addr vec
    -- readWord ::
    --   forall wordSize addrWidth w.
    --   ( KnownNat wordSize,
    --     KnownNat addrWidth,
    --     KnownNat w,
    --     CmpNat (w + 1) wordSize ~ GT
    --   ) =>
    --   Unsigned addrWidth ->
    --   Vec w Bit ->
    --   BitVector wordSize
    readWord' addr v = bitCoerce wrd
      where
        wordAsUnsigned = natToNum @wordSize @(Unsigned wordSize)
        rotateDist = mul addr wordAsUnsigned
        v' = rotateLeft v rotateDist
        wrd = withSNat @wordSize (\n -> select d0 d1 n v')


writeWord :: forall wordSize addrWidth w.
  (KnownNat wordSize, KnownNat addrWidth, KnownNat w)
  => Address addrWidth -> BitVector wordSize -> FibState' w -> FibState' w
writeWord = undefined

getVector :: (Monad m) => AddressType -> S.StateT (FibState' w) m (LFSR w)
getVector LFSR = S.gets reg
getVector Taps = S.gets taps

fibThingie ::
  forall wordSize addrWidth nWords.
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
  let v = rotateLeft vec addr -- mit der Wortbreite rotieren?
      w = withSNat @wordSize (\n -> select d0 d1 n v)
  return $ bitCoerce w
fibThingie (Write add val) = do
  return 0

initState =
  FibState'
    { reg = repeat 1 :: LFSR 8,
      taps = $(listToVecTH $ genList 8 [8, 6, 5, 4]),
      mode = Running
    }

ml :: (HiddenClockResetEnable System) => Signal System (FibCmd 4 1) -> Signal System (BitVector 4)
ml = mealyS (fibThingie @4 @1) initState

-- getVector :: forall wordSize nWords. (KnownNat wordSize, KnownNat nWords) => AddressType -> FibState wordSize nWords (Vec (wordSize * nWords) Bit)
