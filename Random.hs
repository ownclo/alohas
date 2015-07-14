module Random where

import System.Random( StdGen, randoms )

randomBools :: Double -> StdGen -> [Bool]
randomBools pTrue = map (<= pTrue) . randoms
