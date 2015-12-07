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
       do
       -- forM_ [0.0, 0.1 .. 0.5] $ \lambda -> do
       -- forM_ [0, 1 .. 9] $ \nsteps -> do
        let y = lambda; lambda = 1.0
        --  let y = lambda -- / fromIntegral nusers
        -- nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        let genBools = map rstreams gens
            rstreams = randomBools y *** randomBools p
            uids = UID <$> [1..]
            params = zip uids (repeat ())
            -- params = zip uids (repeat INF)
            -- userParams = zip params genBools
            userParams = [((UID 1, ()), ([True] ++ repeat False, [True,  False, True,  False, True]))
                         ,((UID 2, ()), ([True] ++ repeat False, [True,  False, False, False, False]))
                         -- ,((UID 3, ()), ([True] ++ repeat False, [False, True,  True]))
                         ]
            usrs = map initUser userParams
            model = presentModel nsteps userParams $ runModel nsteps usrs
            mDelay = meanDelay $ model^.users
            dlays = sum $ map (fromIntegral . view delays) $ model^.users :: Integer
            nmsgs = sum $ map (fromIntegral . view numMsgs) $ model^.users :: Integer
        void $ printf "%.2f\t%.5f\t%d\t%d\n" lambda (mDelay :: Double) dlays nmsgs
        print $ model^.stats
        mapM_ print $ model^.users
        putStrLn ""
    where nusers = 4
          nsteps = 7
          p = 0.5 -- for Tree Algorithms
