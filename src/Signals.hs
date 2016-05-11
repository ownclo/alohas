module Signals where

import Data.List( (\\) )

import Interface

addSignals :: ForwSignal -> ForwSignal -> ForwSignal
addSignals (ForwSignal u1 n1) (ForwSignal u2 n2) =
        ForwSignal (u1 ++ u2) (n1 + n2)

-- NB: Noise is getting accumulated upon subtraction
subtractSignals :: ForwSignal -> ForwSignal -> ForwSignal
subtractSignals (ForwSignal u1 n1) (ForwSignal u2 n2) =
        ForwSignal (u1 \\ u2) (n1 + n2)

clearNoise :: ForwSignal -> ForwSignal
clearNoise (ForwSignal u _) = ForwSignal u 0.0
