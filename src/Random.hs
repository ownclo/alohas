module Random where

import System.Random( StdGen, randoms, split )
import Data.List( unfoldr )
import Data.Random.Distribution.Poisson( poisson )
import Data.Random

randomBools :: Double -> StdGen -> [Bool]
randomBools pTrue = map (<= pTrue) . randoms

newBoolsStream :: Double -> StdGen -> ([Bool], StdGen)
newBoolsStream pTrue gen = (randomBools pTrue g, g')
    where (g, g') = split gen

poissonStream :: Double -> StdGen -> [Int]
poissonStream lambda = unfoldr (Just . sampleState dist)
    where dist = poisson lambda
