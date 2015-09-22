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
main = do --forM_ [0.01, 0.02 .. 1.0] $ \y -> do
        let y = 1.0
        -- nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        let genBools = map rstreams gens
            rstreams = randomBools y *** randomBools p
            uids = UID <$> [1..]
            params = zip uids (repeat ())
            userParams = zip params genBools
            -- userParams = [((UID 1, INF), ([True, True, True, True, True], [True,True,True])),
            --               ((UID 2, INF), ([True, True, True, True, True], [True, False, False]))]
            usrs = map initUser userParams
            model = presentModel nsteps userParams $ runModel nsteps usrs
            mDelay = meanDelay $ model^.users
        void $ printf "%.2f\t%.5f\n" y (mDelay :: Double)
        print $ model^.stats
        mapM_ print $ model^.users
    where nusers = 2
          nsteps = 5
          p = 0.5 -- for Tree Algorithms
