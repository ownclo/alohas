module Signals where

import Data.List( (\\) )

import Interface

add :: ForwSignal -> ForwSignal -> ForwSignal
add (ForwSignal u1 n1) (ForwSignal u2 n2) =
        ForwSignal (u1 ++ u2) (n1 + n2)

-- NB: Noise is getting accumulated upon subtraction
subtract :: ForwSignal -> ForwSignal -> ForwSignal
subtract (ForwSignal u1 n1) (ForwSignal u2 n2)
    | n2 /= 0.0 = error $ "OUR ALGORITHM ONLY!: " ++ show n2
    | otherwise = ForwSignal (u1 \\ u2) (n1 + n2)

clearNoise :: ForwSignal -> ForwSignal
clearNoise (ForwSignal u _) = ForwSignal u 0.0

getNoise :: ForwSignal -> Double
getNoise (ForwSignal _ n) = n
