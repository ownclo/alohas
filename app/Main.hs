module Main where

import Control.Monad
import Control.Applicative
import Control.Arrow( (***) )
import Control.Lens

-- import System.Environment( getArgs )
import System.Random( newStdGen, split )
import Text.Printf( printf )

import Model
import User
import Random( randomBools )

main :: IO ()
main = forM_ [0.01, 0.02 .. 1.0] $ \y -> do
        -- nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        let userParams = map rstreams gens
            rstreams = randomBools y *** randomBools p
            usrs = map initUser userParams
            model = {- presentModel nsteps userParams $ -} runModel nsteps usrs
            mDelay = meanDelay $ model^.users
        void $ printf "%.2f\t%.5f\n" y (mDelay :: Double)
        -- print $ model^.stats
        -- mapM_ print $ model^.users
        return ()
    where nusers = 2
          nsteps = 100000
          p = 1.0 / fromIntegral nusers
