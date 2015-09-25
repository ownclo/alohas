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
import Interface( UserID(..), MsgQueueLen(..) )

main :: IO ()
main =
       -- forM_ [0.01, 0.11 .. 0.51] $ \lambda -> do
       forM_ [0, 1 .. 7] $ \nsteps -> do
        let y = 0.3
        -- let y = lambda / fromIntegral nusers
        -- nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        let genBools = map rstreams gens
            rstreams = randomBools y *** randomBools p
            uids = UID <$> [1..]
            params = zip uids (repeat ())
            userParams = zip params genBools
            -- userParams = [((UID 1, ()), ([False, False, False, False, False, False, False], [False,True,True]))
            --              ,((UID 2, ()), ([False, False, False, False, False, False, False], [False, False, False]))
            --              ,((UID 3, ()), ([True, True, True, True, True], [True, True, True]))
            --              ]
            usrs = map initUser userParams
            model = presentModel nsteps userParams $ runModel nsteps usrs
            mDelay = meanDelay $ model^.users
        -- void $ printf "%.2f\t%.5f\n" lambda (mDelay :: Double)
        print $ model^.stats
        mapM_ print $ model^.users
        putStrLn ""
    where nusers = 3
          nsteps = 10
          p = 0.5 -- for Tree Algorithms
