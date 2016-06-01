module Signals where

import Data.List( (\\) )
import Data.Number.Erf

import Interface

add :: Double -> ForwSignal -> ForwSignal -> ForwSignal
add subSnr (ForwSignal u1 n1) (ForwSignal u2 n2) =
        ForwSignal (u1 ++ u2) (n1 + n2 + subSnr)

-- NB: Noise is getting accumulated upon subtraction
subtract :: ForwSignal -> ForwSignal -> Double -> ForwSignal
subtract (ForwSignal u1 n1) (ForwSignal u2 n2) subSnr = ForwSignal (u1 \\ u2) (n1 + n2 + subSnr)

clearNoise :: ForwSignal -> ForwSignal
clearNoise (ForwSignal u _) = ForwSignal u 0.0

getNoise :: ForwSignal -> Double
getNoise (ForwSignal _ n) = n

probErrorSignal :: ForwSignal -> Double -> Integer -> Double
probErrorSignal (ForwSignal _ 0.0) _baseSnr _l = 0.0
-- probErrorSignal (ForwSignal _ 1.0) baseSnr l = probError baseSnr l
-- probErrorSignal (ForwSignal _ 2.0) baseSnr l = probError (baseSnr / 2.0) l
probErrorSignal (ForwSignal _ n) baseSnr l = probError snr l
  where snr = baseSnr / n
-- probErrorSignal f _ _ = error $ "IMPOSSIBLE! " ++ show f


probError :: Double -> Integer -> Double
probError snr l = 1 - (1 - probErrorBit snr) ** fromIntegral l

probErrorDb :: Double -> Integer -> Double
probErrorDb snrDb = probError (fromDb snrDb)

probErrorBit :: Double -> Double
probErrorBit = half . erfc . sqrt
    where half = (/2.0)

probErrorBitDb :: Double -> Double
probErrorBitDb = probErrorBitDb . fromDb

toDb :: Double -> Double
toDb x = 10 * logBase 10 x

fromDb :: Double -> Double
fromDb db = 10 ** (db / 10.0)
