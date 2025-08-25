module LFSR.Fibonacci.MaxLenTaps where

import Clash.Prelude
import qualified Data.List as L
import LFSR
import LFSR.Fibonacci
import LFSR.Fibonacci.TH

import qualified Data.IntMap.Strict as IntMap

import qualified Data.List as L

--examples taken from https://en.wikipedia.org/wiki/Linear-feedback_shift_register#Example_polynomials_for_maximal_LFSRs


-- m n' = do
--   pure []
--   where
--     n = show n'
--     methodName = mkName $ "maxLenTaps" L.++ n'

-- -- das hier funktioniert _vielleicht_, muss aber in eine andere Datei. Ich dreh' durch


fibLFSR2 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 2)
fibLFSR2 = fibLFSR $(listToVecTH $ getMaxLenTaps 2) (repeat (1 :: Bit))

fibLFSR3 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 3)
fibLFSR3 = fibLFSR $(listToVecTH $ getMaxLenTaps 3) (repeat (1 :: Bit))

fibLFSR4 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 4)
fibLFSR4 = fibLFSR $(listToVecTH $ getMaxLenTaps 4) (repeat (1 :: Bit))

fibLFSR5 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 5)
fibLFSR5 = fibLFSR $(listToVecTH $ getMaxLenTaps 5) (repeat (1 :: Bit))

fibLFSR6 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 6)
fibLFSR6 = fibLFSR $(listToVecTH $ getMaxLenTaps 6) (repeat (1 :: Bit))

fibLFSR7 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 7)
fibLFSR7 = fibLFSR $(listToVecTH $ getMaxLenTaps 7) (repeat (1 :: Bit))

fibLFSR8 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 8)
fibLFSR8 = fibLFSR $(listToVecTH $ getMaxLenTaps 8) (repeat (1 :: Bit))

fibLFSR9 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 9)
fibLFSR9 = fibLFSR $(listToVecTH $ getMaxLenTaps 9) (repeat (1 :: Bit))

fibLFSR10 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 10)
fibLFSR10 = fibLFSR $(listToVecTH $ getMaxLenTaps 10) (repeat (1 :: Bit))

fibLFSR11 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 11)
fibLFSR11 = fibLFSR $(listToVecTH $ getMaxLenTaps 11) (repeat (1 :: Bit))

fibLFSR12 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 12)
fibLFSR12 = fibLFSR $(listToVecTH $ getMaxLenTaps 12) (repeat (1 :: Bit))

fibLFSR13 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 13)
fibLFSR13 = fibLFSR $(listToVecTH $ getMaxLenTaps 13) (repeat (1 :: Bit))

fibLFSR14 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 14)
fibLFSR14 = fibLFSR $(listToVecTH $ getMaxLenTaps 14) (repeat (1 :: Bit))

fibLFSR15 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 15)
fibLFSR15 = fibLFSR $(listToVecTH $ getMaxLenTaps 15) (repeat (1 :: Bit))

fibLFSR16 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 16)
fibLFSR16 = fibLFSR $(listToVecTH $ getMaxLenTaps 16) (repeat (1 :: Bit))

fibLFSR17 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 17)
fibLFSR17 = fibLFSR $(listToVecTH $ getMaxLenTaps 17) (repeat (1 :: Bit))

fibLFSR18 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 18)
fibLFSR18 = fibLFSR $(listToVecTH $ getMaxLenTaps 18) (repeat (1 :: Bit))

fibLFSR19 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 19)
fibLFSR19 = fibLFSR $(listToVecTH $ getMaxLenTaps 19) (repeat (1 :: Bit))

fibLFSR20 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 20)
fibLFSR20 = fibLFSR $(listToVecTH $ getMaxLenTaps 20) (repeat (1 :: Bit))

fibLFSR21 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 21)
fibLFSR21 = fibLFSR $(listToVecTH $ getMaxLenTaps 21) (repeat (1 :: Bit))

fibLFSR22 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 22)
fibLFSR22 = fibLFSR $(listToVecTH $ getMaxLenTaps 22) (repeat (1 :: Bit))

fibLFSR23 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 23)
fibLFSR23 = fibLFSR $(listToVecTH $ getMaxLenTaps 23) (repeat (1 :: Bit))

fibLFSR24 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 24)
fibLFSR24 = fibLFSR $(listToVecTH $ getMaxLenTaps 24) (repeat (1 :: Bit))

fibLFSR25 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 25)
fibLFSR25 = fibLFSR $(listToVecTH $ getMaxLenTaps 25) (repeat (1 :: Bit))

fibLFSR26 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 26)
fibLFSR26 = fibLFSR $(listToVecTH $ getMaxLenTaps 26) (repeat (1 :: Bit))

fibLFSR27 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 27)
fibLFSR27 = fibLFSR $(listToVecTH $ getMaxLenTaps 27) (repeat (1 :: Bit))

fibLFSR28 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 28)
fibLFSR28 = fibLFSR $(listToVecTH $ getMaxLenTaps 28) (repeat (1 :: Bit))

fibLFSR29 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 29)
fibLFSR29 = fibLFSR $(listToVecTH $ getMaxLenTaps 29) (repeat (1 :: Bit))

fibLFSR30 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 30)
fibLFSR30 = fibLFSR $(listToVecTH $ getMaxLenTaps 30) (repeat (1 :: Bit))

fibLFSR31 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 31)
fibLFSR31 = fibLFSR $(listToVecTH $ getMaxLenTaps 31) (repeat (1 :: Bit))

fibLFSR32 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 32)
fibLFSR32 = fibLFSR $(listToVecTH $ getMaxLenTaps 32) (repeat (1 :: Bit))

fibLFSR33 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 33)
fibLFSR33 = fibLFSR $(listToVecTH $ getMaxLenTaps 33) (repeat (1 :: Bit))

fibLFSR34 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 34)
fibLFSR34 = fibLFSR $(listToVecTH $ getMaxLenTaps 34) (repeat (1 :: Bit))

fibLFSR35 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 35)
fibLFSR35 = fibLFSR $(listToVecTH $ getMaxLenTaps 35) (repeat (1 :: Bit))

fibLFSR36 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 36)
fibLFSR36 = fibLFSR $(listToVecTH $ getMaxLenTaps 36) (repeat (1 :: Bit))

fibLFSR37 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 37)
fibLFSR37 = fibLFSR $(listToVecTH $ getMaxLenTaps 37) (repeat (1 :: Bit))

fibLFSR38 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 38)
fibLFSR38 = fibLFSR $(listToVecTH $ getMaxLenTaps 38) (repeat (1 :: Bit))

fibLFSR39 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 39)
fibLFSR39 = fibLFSR $(listToVecTH $ getMaxLenTaps 39) (repeat (1 :: Bit))

fibLFSR40 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 40)
fibLFSR40 = fibLFSR $(listToVecTH $ getMaxLenTaps 40) (repeat (1 :: Bit))

fibLFSR41 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 41)
fibLFSR41 = fibLFSR $(listToVecTH $ getMaxLenTaps 41) (repeat (1 :: Bit))

fibLFSR42 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 42)
fibLFSR42 = fibLFSR $(listToVecTH $ getMaxLenTaps 42) (repeat (1 :: Bit))

fibLFSR43 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 43)
fibLFSR43 = fibLFSR $(listToVecTH $ getMaxLenTaps 43) (repeat (1 :: Bit))

fibLFSR44 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 44)
fibLFSR44 = fibLFSR $(listToVecTH $ getMaxLenTaps 44) (repeat (1 :: Bit))

fibLFSR45 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 45)
fibLFSR45 = fibLFSR $(listToVecTH $ getMaxLenTaps 45) (repeat (1 :: Bit))

fibLFSR46 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 46)
fibLFSR46 = fibLFSR $(listToVecTH $ getMaxLenTaps 46) (repeat (1 :: Bit))

fibLFSR47 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 47)
fibLFSR47 = fibLFSR $(listToVecTH $ getMaxLenTaps 47) (repeat (1 :: Bit))

fibLFSR48 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 48)
fibLFSR48 = fibLFSR $(listToVecTH $ getMaxLenTaps 48) (repeat (1 :: Bit))

fibLFSR49 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 49)
fibLFSR49 = fibLFSR $(listToVecTH $ getMaxLenTaps 49) (repeat (1 :: Bit))

fibLFSR50 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 50)
fibLFSR50 = fibLFSR $(listToVecTH $ getMaxLenTaps 50) (repeat (1 :: Bit))

fibLFSR51 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 51)
fibLFSR51 = fibLFSR $(listToVecTH $ getMaxLenTaps 51) (repeat (1 :: Bit))

fibLFSR52 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 52)
fibLFSR52 = fibLFSR $(listToVecTH $ getMaxLenTaps 52) (repeat (1 :: Bit))

fibLFSR53 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 53)
fibLFSR53 = fibLFSR $(listToVecTH $ getMaxLenTaps 53) (repeat (1 :: Bit))

fibLFSR54 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 54)
fibLFSR54 = fibLFSR $(listToVecTH $ getMaxLenTaps 54) (repeat (1 :: Bit))

fibLFSR55 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 55)
fibLFSR55 = fibLFSR $(listToVecTH $ getMaxLenTaps 55) (repeat (1 :: Bit))

fibLFSR56 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 56)
fibLFSR56 = fibLFSR $(listToVecTH $ getMaxLenTaps 56) (repeat (1 :: Bit))

fibLFSR57 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 57)
fibLFSR57 = fibLFSR $(listToVecTH $ getMaxLenTaps 57) (repeat (1 :: Bit))

fibLFSR58 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 58)
fibLFSR58 = fibLFSR $(listToVecTH $ getMaxLenTaps 58) (repeat (1 :: Bit))

fibLFSR59 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 59)
fibLFSR59 = fibLFSR $(listToVecTH $ getMaxLenTaps 59) (repeat (1 :: Bit))

fibLFSR60 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 60)
fibLFSR60 = fibLFSR $(listToVecTH $ getMaxLenTaps 60) (repeat (1 :: Bit))

fibLFSR61 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 61)
fibLFSR61 = fibLFSR $(listToVecTH $ getMaxLenTaps 61) (repeat (1 :: Bit))

fibLFSR62 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 62)
fibLFSR62 = fibLFSR $(listToVecTH $ getMaxLenTaps 62) (repeat (1 :: Bit))

fibLFSR63 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 63)
fibLFSR63 = fibLFSR $(listToVecTH $ getMaxLenTaps 63) (repeat (1 :: Bit))

fibLFSR64 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 64)
fibLFSR64 = fibLFSR $(listToVecTH $ getMaxLenTaps 64) (repeat (1 :: Bit))

fibLFSR65 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 65)
fibLFSR65 = fibLFSR $(listToVecTH $ getMaxLenTaps 65) (repeat (1 :: Bit))

fibLFSR66 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 66)
fibLFSR66 = fibLFSR $(listToVecTH $ getMaxLenTaps 66) (repeat (1 :: Bit))

fibLFSR67 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 67)
fibLFSR67 = fibLFSR $(listToVecTH $ getMaxLenTaps 67) (repeat (1 :: Bit))

fibLFSR68 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 68)
fibLFSR68 = fibLFSR $(listToVecTH $ getMaxLenTaps 68) (repeat (1 :: Bit))

fibLFSR69 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 69)
fibLFSR69 = fibLFSR $(listToVecTH $ getMaxLenTaps 69) (repeat (1 :: Bit))

fibLFSR70 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 70)
fibLFSR70 = fibLFSR $(listToVecTH $ getMaxLenTaps 70) (repeat (1 :: Bit))

fibLFSR71 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 71)
fibLFSR71 = fibLFSR $(listToVecTH $ getMaxLenTaps 71) (repeat (1 :: Bit))

fibLFSR72 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 72)
fibLFSR72 = fibLFSR $(listToVecTH $ getMaxLenTaps 72) (repeat (1 :: Bit))

fibLFSR73 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 73)
fibLFSR73 = fibLFSR $(listToVecTH $ getMaxLenTaps 73) (repeat (1 :: Bit))

fibLFSR74 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 74)
fibLFSR74 = fibLFSR $(listToVecTH $ getMaxLenTaps 74) (repeat (1 :: Bit))

fibLFSR75 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 75)
fibLFSR75 = fibLFSR $(listToVecTH $ getMaxLenTaps 75) (repeat (1 :: Bit))

fibLFSR76 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 76)
fibLFSR76 = fibLFSR $(listToVecTH $ getMaxLenTaps 76) (repeat (1 :: Bit))

fibLFSR77 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 77)
fibLFSR77 = fibLFSR $(listToVecTH $ getMaxLenTaps 77) (repeat (1 :: Bit))

fibLFSR78 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 78)
fibLFSR78 = fibLFSR $(listToVecTH $ getMaxLenTaps 78) (repeat (1 :: Bit))

fibLFSR79 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 79)
fibLFSR79 = fibLFSR $(listToVecTH $ getMaxLenTaps 79) (repeat (1 :: Bit))

fibLFSR80 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 80)
fibLFSR80 = fibLFSR $(listToVecTH $ getMaxLenTaps 80) (repeat (1 :: Bit))

fibLFSR81 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 81)
fibLFSR81 = fibLFSR $(listToVecTH $ getMaxLenTaps 81) (repeat (1 :: Bit))

fibLFSR82 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 82)
fibLFSR82 = fibLFSR $(listToVecTH $ getMaxLenTaps 82) (repeat (1 :: Bit))

fibLFSR83 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 83)
fibLFSR83 = fibLFSR $(listToVecTH $ getMaxLenTaps 83) (repeat (1 :: Bit))

fibLFSR84 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 84)
fibLFSR84 = fibLFSR $(listToVecTH $ getMaxLenTaps 84) (repeat (1 :: Bit))

fibLFSR85 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 85)
fibLFSR85 = fibLFSR $(listToVecTH $ getMaxLenTaps 85) (repeat (1 :: Bit))

fibLFSR86 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 86)
fibLFSR86 = fibLFSR $(listToVecTH $ getMaxLenTaps 86) (repeat (1 :: Bit))

fibLFSR87 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 87)
fibLFSR87 = fibLFSR $(listToVecTH $ getMaxLenTaps 87) (repeat (1 :: Bit))

fibLFSR88 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 88)
fibLFSR88 = fibLFSR $(listToVecTH $ getMaxLenTaps 88) (repeat (1 :: Bit))

fibLFSR89 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 89)
fibLFSR89 = fibLFSR $(listToVecTH $ getMaxLenTaps 89) (repeat (1 :: Bit))

fibLFSR90 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 90)
fibLFSR90 = fibLFSR $(listToVecTH $ getMaxLenTaps 90) (repeat (1 :: Bit))

fibLFSR91 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 91)
fibLFSR91 = fibLFSR $(listToVecTH $ getMaxLenTaps 91) (repeat (1 :: Bit))

fibLFSR92 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 92)
fibLFSR92 = fibLFSR $(listToVecTH $ getMaxLenTaps 92) (repeat (1 :: Bit))

fibLFSR93 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 93)
fibLFSR93 = fibLFSR $(listToVecTH $ getMaxLenTaps 93) (repeat (1 :: Bit))

fibLFSR94 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 94)
fibLFSR94 = fibLFSR $(listToVecTH $ getMaxLenTaps 94) (repeat (1 :: Bit))

fibLFSR95 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 95)
fibLFSR95 = fibLFSR $(listToVecTH $ getMaxLenTaps 95) (repeat (1 :: Bit))

fibLFSR96 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 96)
fibLFSR96 = fibLFSR $(listToVecTH $ getMaxLenTaps 96) (repeat (1 :: Bit))

fibLFSR97 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 97)
fibLFSR97 = fibLFSR $(listToVecTH $ getMaxLenTaps 97) (repeat (1 :: Bit))

fibLFSR98 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 98)
fibLFSR98 = fibLFSR $(listToVecTH $ getMaxLenTaps 98) (repeat (1 :: Bit))

fibLFSR99 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 99)
fibLFSR99 = fibLFSR $(listToVecTH $ getMaxLenTaps 99) (repeat (1 :: Bit))

fibLFSR100 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 100)
fibLFSR100 = fibLFSR $(listToVecTH $ getMaxLenTaps 100) (repeat (1 :: Bit))

fibLFSR101 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 101)
fibLFSR101 = fibLFSR $(listToVecTH $ getMaxLenTaps 101) (repeat (1 :: Bit))

fibLFSR102 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 102)
fibLFSR102 = fibLFSR $(listToVecTH $ getMaxLenTaps 102) (repeat (1 :: Bit))

fibLFSR103 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 103)
fibLFSR103 = fibLFSR $(listToVecTH $ getMaxLenTaps 103) (repeat (1 :: Bit))

fibLFSR104 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 104)
fibLFSR104 = fibLFSR $(listToVecTH $ getMaxLenTaps 104) (repeat (1 :: Bit))

fibLFSR105 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 105)
fibLFSR105 = fibLFSR $(listToVecTH $ getMaxLenTaps 105) (repeat (1 :: Bit))

fibLFSR106 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 106)
fibLFSR106 = fibLFSR $(listToVecTH $ getMaxLenTaps 106) (repeat (1 :: Bit))

fibLFSR107 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 107)
fibLFSR107 = fibLFSR $(listToVecTH $ getMaxLenTaps 107) (repeat (1 :: Bit))

fibLFSR108 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 108)
fibLFSR108 = fibLFSR $(listToVecTH $ getMaxLenTaps 108) (repeat (1 :: Bit))

fibLFSR109 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 109)
fibLFSR109 = fibLFSR $(listToVecTH $ getMaxLenTaps 109) (repeat (1 :: Bit))

fibLFSR110 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 110)
fibLFSR110 = fibLFSR $(listToVecTH $ getMaxLenTaps 110) (repeat (1 :: Bit))

fibLFSR111 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 111)
fibLFSR111 = fibLFSR $(listToVecTH $ getMaxLenTaps 111) (repeat (1 :: Bit))

fibLFSR112 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 112)
fibLFSR112 = fibLFSR $(listToVecTH $ getMaxLenTaps 112) (repeat (1 :: Bit))

fibLFSR113 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 113)
fibLFSR113 = fibLFSR $(listToVecTH $ getMaxLenTaps 113) (repeat (1 :: Bit))

fibLFSR114 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 114)
fibLFSR114 = fibLFSR $(listToVecTH $ getMaxLenTaps 114) (repeat (1 :: Bit))

fibLFSR115 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 115)
fibLFSR115 = fibLFSR $(listToVecTH $ getMaxLenTaps 115) (repeat (1 :: Bit))

fibLFSR116 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 116)
fibLFSR116 = fibLFSR $(listToVecTH $ getMaxLenTaps 116) (repeat (1 :: Bit))

fibLFSR117 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 117)
fibLFSR117 = fibLFSR $(listToVecTH $ getMaxLenTaps 117) (repeat (1 :: Bit))

fibLFSR118 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 118)
fibLFSR118 = fibLFSR $(listToVecTH $ getMaxLenTaps 118) (repeat (1 :: Bit))

fibLFSR119 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 119)
fibLFSR119 = fibLFSR $(listToVecTH $ getMaxLenTaps 119) (repeat (1 :: Bit))

fibLFSR120 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 120)
fibLFSR120 = fibLFSR $(listToVecTH $ getMaxLenTaps 120) (repeat (1 :: Bit))

fibLFSR121 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 121)
fibLFSR121 = fibLFSR $(listToVecTH $ getMaxLenTaps 121) (repeat (1 :: Bit))

fibLFSR122 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 122)
fibLFSR122 = fibLFSR $(listToVecTH $ getMaxLenTaps 122) (repeat (1 :: Bit))

fibLFSR123 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 123)
fibLFSR123 = fibLFSR $(listToVecTH $ getMaxLenTaps 123) (repeat (1 :: Bit))

fibLFSR124 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 124)
fibLFSR124 = fibLFSR $(listToVecTH $ getMaxLenTaps 124) (repeat (1 :: Bit))

fibLFSR125 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 125)
fibLFSR125 = fibLFSR $(listToVecTH $ getMaxLenTaps 125) (repeat (1 :: Bit))

fibLFSR126 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 126)
fibLFSR126 = fibLFSR $(listToVecTH $ getMaxLenTaps 126) (repeat (1 :: Bit))

fibLFSR127 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 127)
fibLFSR127 = fibLFSR $(listToVecTH $ getMaxLenTaps 127) (repeat (1 :: Bit))

fibLFSR128 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 128)
fibLFSR128 = fibLFSR $(listToVecTH $ getMaxLenTaps 128) (repeat (1 :: Bit))

fibLFSR129 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 129)
fibLFSR129 = fibLFSR $(listToVecTH $ getMaxLenTaps 129) (repeat (1 :: Bit))

fibLFSR130 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 130)
fibLFSR130 = fibLFSR $(listToVecTH $ getMaxLenTaps 130) (repeat (1 :: Bit))

fibLFSR131 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 131)
fibLFSR131 = fibLFSR $(listToVecTH $ getMaxLenTaps 131) (repeat (1 :: Bit))

fibLFSR132 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 132)
fibLFSR132 = fibLFSR $(listToVecTH $ getMaxLenTaps 132) (repeat (1 :: Bit))

fibLFSR133 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 133)
fibLFSR133 = fibLFSR $(listToVecTH $ getMaxLenTaps 133) (repeat (1 :: Bit))

fibLFSR134 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 134)
fibLFSR134 = fibLFSR $(listToVecTH $ getMaxLenTaps 134) (repeat (1 :: Bit))

fibLFSR135 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 135)
fibLFSR135 = fibLFSR $(listToVecTH $ getMaxLenTaps 135) (repeat (1 :: Bit))

fibLFSR136 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 136)
fibLFSR136 = fibLFSR $(listToVecTH $ getMaxLenTaps 136) (repeat (1 :: Bit))

fibLFSR137 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 137)
fibLFSR137 = fibLFSR $(listToVecTH $ getMaxLenTaps 137) (repeat (1 :: Bit))

fibLFSR138 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 138)
fibLFSR138 = fibLFSR $(listToVecTH $ getMaxLenTaps 138) (repeat (1 :: Bit))

fibLFSR139 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 139)
fibLFSR139 = fibLFSR $(listToVecTH $ getMaxLenTaps 139) (repeat (1 :: Bit))

fibLFSR140 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 140)
fibLFSR140 = fibLFSR $(listToVecTH $ getMaxLenTaps 140) (repeat (1 :: Bit))

fibLFSR141 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 141)
fibLFSR141 = fibLFSR $(listToVecTH $ getMaxLenTaps 141) (repeat (1 :: Bit))

fibLFSR142 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 142)
fibLFSR142 = fibLFSR $(listToVecTH $ getMaxLenTaps 142) (repeat (1 :: Bit))

fibLFSR143 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 143)
fibLFSR143 = fibLFSR $(listToVecTH $ getMaxLenTaps 143) (repeat (1 :: Bit))

fibLFSR144 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 144)
fibLFSR144 = fibLFSR $(listToVecTH $ getMaxLenTaps 144) (repeat (1 :: Bit))

fibLFSR145 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 145)
fibLFSR145 = fibLFSR $(listToVecTH $ getMaxLenTaps 145) (repeat (1 :: Bit))

fibLFSR146 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 146)
fibLFSR146 = fibLFSR $(listToVecTH $ getMaxLenTaps 146) (repeat (1 :: Bit))

fibLFSR147 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 147)
fibLFSR147 = fibLFSR $(listToVecTH $ getMaxLenTaps 147) (repeat (1 :: Bit))

fibLFSR148 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 148)
fibLFSR148 = fibLFSR $(listToVecTH $ getMaxLenTaps 148) (repeat (1 :: Bit))

fibLFSR149 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 149)
fibLFSR149 = fibLFSR $(listToVecTH $ getMaxLenTaps 149) (repeat (1 :: Bit))

fibLFSR150 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 150)
fibLFSR150 = fibLFSR $(listToVecTH $ getMaxLenTaps 150) (repeat (1 :: Bit))

fibLFSR151 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 151)
fibLFSR151 = fibLFSR $(listToVecTH $ getMaxLenTaps 151) (repeat (1 :: Bit))

fibLFSR152 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 152)
fibLFSR152 = fibLFSR $(listToVecTH $ getMaxLenTaps 152) (repeat (1 :: Bit))

fibLFSR153 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 153)
fibLFSR153 = fibLFSR $(listToVecTH $ getMaxLenTaps 153) (repeat (1 :: Bit))

fibLFSR154 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 154)
fibLFSR154 = fibLFSR $(listToVecTH $ getMaxLenTaps 154) (repeat (1 :: Bit))

fibLFSR155 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 155)
fibLFSR155 = fibLFSR $(listToVecTH $ getMaxLenTaps 155) (repeat (1 :: Bit))

fibLFSR156 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 156)
fibLFSR156 = fibLFSR $(listToVecTH $ getMaxLenTaps 156) (repeat (1 :: Bit))

fibLFSR157 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 157)
fibLFSR157 = fibLFSR $(listToVecTH $ getMaxLenTaps 157) (repeat (1 :: Bit))

fibLFSR158 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 158)
fibLFSR158 = fibLFSR $(listToVecTH $ getMaxLenTaps 158) (repeat (1 :: Bit))

fibLFSR159 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 159)
fibLFSR159 = fibLFSR $(listToVecTH $ getMaxLenTaps 159) (repeat (1 :: Bit))

fibLFSR160 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 160)
fibLFSR160 = fibLFSR $(listToVecTH $ getMaxLenTaps 160) (repeat (1 :: Bit))

fibLFSR161 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 161)
fibLFSR161 = fibLFSR $(listToVecTH $ getMaxLenTaps 161) (repeat (1 :: Bit))

fibLFSR162 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 162)
fibLFSR162 = fibLFSR $(listToVecTH $ getMaxLenTaps 162) (repeat (1 :: Bit))

fibLFSR163 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 163)
fibLFSR163 = fibLFSR $(listToVecTH $ getMaxLenTaps 163) (repeat (1 :: Bit))

fibLFSR164 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 164)
fibLFSR164 = fibLFSR $(listToVecTH $ getMaxLenTaps 164) (repeat (1 :: Bit))

fibLFSR165 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 165)
fibLFSR165 = fibLFSR $(listToVecTH $ getMaxLenTaps 165) (repeat (1 :: Bit))

fibLFSR166 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 166)
fibLFSR166 = fibLFSR $(listToVecTH $ getMaxLenTaps 166) (repeat (1 :: Bit))

fibLFSR167 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 167)
fibLFSR167 = fibLFSR $(listToVecTH $ getMaxLenTaps 167) (repeat (1 :: Bit))

fibLFSR168 :: (HiddenClockResetEnable dom) => Signal dom (LFSR 168)
fibLFSR168 = fibLFSR $(listToVecTH $ getMaxLenTaps 168) (repeat (1 :: Bit))
