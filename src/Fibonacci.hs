{-# LANGUAGE ViewPatterns #-}

module Fibonacci where

import Clash.Prelude
import Control.Monad.Extra
import qualified Control.Monad.Trans.State.Strict as S
import Fibonacci.Taps

type Taps n = Vec n Bit
type FibReg n = Vec n Bit

fibonacciLFSR :: (KnownNat n, n ~ k + 1) => Taps n -> FibReg n -> FibReg n
fibonacciLFSR taps reg = reg'
  where
    selection = zipWith (.&.) reg taps
    newBit = fold xor selection
    reg' = newBit +>> reg


fibonacciLFSR' :: (HiddenClockResetEnable dom, KnownNat n, n ~ k + 1) => Taps n -> FibReg n -> Signal dom (FibReg n)
fibonacciLFSR' taps i = reg
  where
    reg = register i (fmap (fibonacciLFSR taps) reg)


data FibMode = Stopped | Running | Explicit deriving (Eq, Show, Generic, NFDataX)

data FS nWords wordSize = FS
  { reg :: Vec nWords (BitVector wordSize),
    taps :: Vec nWords (BitVector wordSize),
    mode :: FibMode
  }
  deriving (Generic, NFDataX)

instance (KnownNat n, KnownNat w) => Show (FS n w) where
  show :: (KnownNat n, KnownNat w) => FS n w -> String
  show (FS {reg, taps, mode}) = "FS\n" <> "\tlfsr: " <> show (v2bv $ bitCoerce reg) <> "\n\ttaps: " <> show (v2bv $ bitCoerce taps) <> "\n\tmode: " <> show mode

data AddressType = FibReg | Taps deriving (Eq, Show, Generic, NFDataX)

data Address nWords = Address AddressType (Index nWords) deriving (Show, Generic, NFDataX)

-- Address ist ein unnötiges Level an Indirektion?
data FibCmd nWords wordSize = Read (Address nWords) | Write (Address nWords) (BitVector wordSize) | SetMode FibMode | Advance | ReadMode | NOP
  deriving (Show, Generic, NFDataX)

readVec :: AddressType -> FS nWords wordSize -> Vec nWords (BitVector wordSize)
readVec addrType s = if addrType == FibReg then reg s else taps s

readWord :: (KnownNat nWords, KnownNat wordSize) => Address nWords -> FS nWords wordSize -> BitVector wordSize
readWord (Address addrType addr) fs = vec !! addr
  where
    vec = readVec addrType fs

setMode :: FibMode -> FS nWords wordSize -> FS nWords wordSize
setMode mode fs = fs {mode = mode}

writeWord :: (KnownNat nWords, KnownNat wordSize) => Address nWords -> BitVector wordSize -> FS nWords wordSize -> FS nWords wordSize
writeWord (Address addrType addr) word fs = case addrType of
  FibReg -> fs {reg = vec'}
  Taps -> fs {taps = vec'}
  where
    vec = readVec addrType fs
    vec' = replace addr word vec

advanceLFSR ::
  forall nWords wordSize k.
  ( KnownNat nWords,
    KnownNat wordSize,
    KnownNat k,
    nWords * wordSize ~ k + 1
  ) =>
  FS nWords wordSize ->
  FS nWords wordSize
advanceLFSR fs@FS {reg, taps} = fs {reg = bitCoerce reg'}
  where
    reg' = fibonacciLFSR tapsFlat regFlat
    -- regFlat :: Vec (n * w) Bit
    regFlat = bitCoerce reg
    -- tapsFlat :: Vec (n * w) Bit
    tapsFlat = bitCoerce taps

initState :: FS 2 4
initState =
  FS
    { reg = bitCoerce (repeat 1 :: FibReg 8),
      taps = bitCoerce $(listToVecTH $ getMaxLenTaps 8),
      mode = Stopped
    }

executeFibonacciLFSRS ::
  forall nWords wordSize k.
  ( KnownNat nWords,
    KnownNat wordSize,
    KnownNat k,
    nWords * wordSize ~ k + 1
  ) =>
  FibCmd nWords wordSize ->
  S.State (FS nWords wordSize) (BitVector wordSize)
executeFibonacciLFSRS (Read addr) = do
  word <- S.gets $ readWord addr
  advance False
  return word

executeFibonacciLFSRS (Write addr word) = do
  advance  False
  S.modify' (writeWord addr word)
  return 0

-- Setting a mode is immediately respected by advance
executeFibonacciLFSRS (SetMode mode) = do
  S.modify' $ \fs -> setMode mode fs
  advance  False
  return 0

executeFibonacciLFSRS ReadMode = do
  advance False
  m <- S.gets mode
  return $ bitify m
    where
      bitify :: FibMode -> BitVector wordSize
      bitify Stopped = 0b00000000
      bitify Running = 0b00000001
      bitify Explicit = 0b00000010


executeFibonacciLFSRS Advance = advance True >> return 0

executeFibonacciLFSRS NOP = advance False >> return 0

advance :: (KnownNat nWords, KnownNat wordSize, KnownNat k, nWords * wordSize ~ (k + 1)) => Bool -> S.State (FS nWords wordSize) ()
advance isAdvanceCmd = do
  mode' <- S.gets mode
  let shouldAdvance = (mode' == Running) || (isAdvanceCmd && mode' /= Stopped)
  when shouldAdvance (S.modify' advanceLFSR)



-- | The domain for the TTQV competition tapeout. It differs from the default `System` domain by having an active low reset.
createDomain vSystem {vName = "TTQV", vResetPolarity = ActiveLow}


-- TODO viel simplere Modulstruktur machen


{-# ANN ttWrapper
  (Synthesize
    { t_name = "fibRNG"
    , t_inputs = [ PortName "clk"
                  , PortName "rst_n"
                  , PortName "ui_in"
                  , PortName "address"
                  , PortName "data_write"
                  , PortName "data_in"
                  ]
    , t_output =  PortProduct "" [ PortName "uo_out"
                  , PortName "data_out"
      ]
    }) #-}
ttWrapper ::
  Clock TTQV ->
  Reset TTQV ->
  Signal TTQV (BitVector 8) ->
  Signal TTQV (BitVector 4) ->
  Signal TTQV Bit ->
  Signal TTQV (BitVector 8) ->
  Signal TTQV (BitVector 8, BitVector 8)
ttWrapper clk rst _ui_in address data_write data_in = bundle (uo_out, design cmd)
  where
    -- we do not make use of the pmod
    uo_out :: Signal TTQV (BitVector 8)
    uo_out = pure 0b00000000
    design :: Signal TTQV (FibCmd 4 8) -> Signal TTQV (BitVector 8)
    design = exposeClockResetEnable (mealyS @TTQV executeFibonacciLFSRS initStateTTQV) clk rst enableGen
    cmd :: Signal TTQV (FibCmd 4 8)
    cmd = liftA3 parseTTQVInput address data_write data_in


initStateTTQV :: FS 4 8
initStateTTQV =  FS
  { reg = bitCoerce (repeat 1 :: FibReg 32),
    taps = bitCoerce $(listToVecTH $ getMaxLenTaps 32),
    mode = Stopped
  }

ttqvm :: Clock TTQV -> Reset TTQV -> Signal TTQV (FibCmd 4 8) -> Signal TTQV (BitVector 8)
ttqvm clk rst = exposeClockResetEnable (mealyS @TTQV executeFibonacciLFSRS initStateTTQV) clk rst enableGen


parseTTQVInput :: BitVector 4 -> Bit -> BitVector 8 -> FibCmd 4 8
parseTTQVInput address data_write data_in =
  if data_write == 1
    then parseWriteCmd address data_in
    else parseReadAddress address

parseReadAddress :: BitVector 4 -> FibCmd 4 8
parseReadAddress $(bitPattern "01aa") = Read (Address Taps (bitCoerce aa))
parseReadAddress $(bitPattern "00aa") = Read (Address FibReg (bitCoerce aa))
parseReadAddress 0b1111 = ReadMode
parseReadAddress _ = NOP

parseWriteCmd :: BitVector 4 -> BitVector 8 -> FibCmd 4 8
parseWriteCmd $(bitPattern "01aa") val = Write (Address Taps (bitCoerce aa)) val
parseWriteCmd $(bitPattern "00aa") val = Write (Address FibReg (bitCoerce aa)) val
parseWriteCmd $(bitPattern "1...") val = case val of
  0b00000000 -> SetMode Stopped
  0b00000001 -> SetMode Running
  0b00000010 -> SetMode Explicit
  0b00000011 -> Advance
  _otherwise -> NOP


