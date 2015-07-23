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
import Interface( UserID(..) )

main :: IO ()
main = do --forM_ [0.01, 0.02 .. 1.0] $ \y -> do
        let y = 1.0
        -- nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        let {- userParams = [(UID 1, [True, True, True, True, True], [True,True,True]),
                          (UID 2, [True, True, True, True, True], [False, False, False])] -}
            genBools = map rstreams gens
            rstreams = randomBools y *** randomBools p
            uids = UID <$> [1..]
            userParams = zip uids genBools
            usrs = map initUser userParams
            model = presentModel nsteps userParams $ runModel nsteps usrs
            mDelay = meanDelay $ model^.users
        void $ printf "%.2f\t%.5f\n" y (mDelay :: Double)
        print $ model^.stats
        mapM_ print $ model^.users
        return ()
    where nusers = 2
          nsteps = 5
          p = 1.0 / fromIntegral nusers
