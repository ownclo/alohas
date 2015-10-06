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
       forM_ [0.5, 0.6 .. 0.9] $ \lambda -> do
       -- forM_ [0, 1 .. 9] $ \nsteps -> do
       --  let y = 0.3
        let y = lambda -- / fromIntegral nusers
        -- nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        let genBools = map rstreams gens
            rstreams = randomBools y *** randomBools p
            uids = UID <$> [1..]
            params = zip uids (repeat ())
            userParams = zip params genBools
            -- userParams = [((UID 1, ()), ([True] ++ repeat False, [True,  False, True,  False, True]))
            --              ,((UID 2, ()), ([True] ++ repeat False, [True,  False, False, False, False]))
            --              ,((UID 3, ()), ([True] ++ repeat False, [False, True,  True]))
            --              ]
            usrs = map initUser userParams
            model = {- presentModel nsteps userParams $ -} runModel nsteps usrs
            mDelay = meanDelay $ model^.users
        void $ printf "%.2f\t%.5f\n" lambda (mDelay :: Double)
        -- print $ model^.stats
        -- mapM_ print $ model^.users
        -- putStrLn ""
    where nusers = 16
          nsteps = 1000000
          p = 0.5 -- for Tree Algorithms
