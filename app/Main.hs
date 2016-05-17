module Main where

import Control.Monad
import Control.Applicative
import Control.Arrow( (***) )
import Control.Lens

-- import System.Environment( getArgs )
import System.Random( newStdGen, split, randoms )
import Text.Printf( printf )

import Model
import User
import Random( randomBools )
import Interface( UserID(..), MsgQueueLen(..) )

main :: IO ()
main = mainPerf

-- mainBounded :: IO ()
-- mainBounded =
--        do
--        -- forM_ [0.0, 0.1 .. 0.5] $ \lambda -> do
--        -- forM_ [0.1, 0.2, 0.3, 0.31, 0.32, 0.33, 0.34] $ \lambda -> do
--        -- forM_ [0, 1 .. 9] $ \nsteps -> do
--         let y = lambda; lambda = 1.0
--         -- let y = lambda / fromIntegral nusers
--         -- nsteps <- read . head <$> getArgs
--         gens <- map split <$> replicateM nusers newStdGen
--         let genBools = map rstreams gens
--             rstreams = randomBools y *** randomBools p
--             uids = UID <$> [1..]
--             -- params = zip uids (repeat ())
--             -- params = zip uids (repeat (Bounded 1))
--             -- userParams = zip params genBools
--             userParams = [((UID 1, ()), ([True, True, False, False, True] ++ repeat False, [True ,  True , True , False, True ]))
--                          ,((UID 2, ()), ([True, True] ++ repeat False, [True ,  False , False, False, False]))
--                          -- ,((UID 3, ()), ([True] ++ repeat False, [False,  False, True ]))
--                          ]
--             usrs = map initUser userParams
--             model = presentModel nsteps userParams $ runModel nsteps usrs
--             mDelay = meanDelay $ model^.users
--             dlays = sum $ map (fromIntegral . view delays) $ model^.users :: Integer
--             nmsgs = sum $ map (fromIntegral . view numMsgs) $ model^.users :: Integer
--         void $ printf "%.2f\t%.5f\t%d\t%d\n" lambda (mDelay :: Double) dlays nmsgs
--         print $ model^.stats
--         mapM_ print $ model^.users
--         putStrLn ""
--     where nusers = 3
--           nsteps = 8
--           p = 0.5 -- for Tree Algorithms

mainPerf :: IO ()
mainPerf =
       do
       -- forM_ [0.85, 0.87, 0.9, 1.0] $ \lambda -> do
       forM_ [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.62, 0.63, 0.65, 0.66, 0.67] $ \lambda -> do
       -- forM_ [0.3, 0.31, 0.32, 0.33, 0.34] $ \lambda -> do
       -- forM_ [0, 1 .. 9] $ \nsteps -> do
        -- let y = lambda; lambda = 1.0
        let y = lambda / fromIntegral nusers
        -- nsteps <- read . head <$> getArgs
        gens <- map split <$> replicateM nusers newStdGen
        -- noiseGen <- randoms <$> newStdGen
        noiseGen <- return $ repeat 0.0
        let genBools = map rstreams gens
            rstreams = randomBools y *** randomBools p
            qs = 0.0
            uids = UID <$> [1..]
            -- params = zip uids (repeat ())
            params = zip uids (repeat INF)
            userParams = zip params genBools
            -- userParams = [((UID 1, ()), ([True, False, False, False, True] ++ repeat False, [True ,  True , True , False, True ]))
            --              ,((UID 2, ()), ([True] ++ repeat False, [True ,  False , False, False, False]))
            --              -- ,((UID 3, ()), ([True] ++ repeat False, [False,  False, True ]))
            --              ]
            usrs = map initUser userParams
            model = {- presentModel nsteps userParams $ -} runModel qs noiseGen nsteps usrs
            mDelay = meanDelay $ model^.users
            dlays = sum $ map (fromIntegral . view delays) $ model^.users :: Integer
            nmsgs = sum $ map (fromIntegral . view numMsgs) $ model^.users :: Integer
        void $ printf "%.2f\t%.5f\t%d\t%d\n" lambda (mDelay + 0.5 :: Double) dlays nmsgs
        -- print $ model^.stats
        -- mapM_ print $ model^.users
        -- putStrLn ""
       putStrLn ""
    where nusers = 100
          nsteps = 1000000
          p = 0.5 -- for Tree Algorithms
