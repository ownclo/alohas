module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
-- import Control.Arrow( (***) )
import Control.Lens
-- import Data.Maybe( fromJust )

-- import System.Environment( getArgs )
import System.Exit( exitSuccess )
import System.Random( newStdGen, {- split, -} randoms )
import Text.Printf( printf )

-- import Model
-- import User
-- import Random( randomBools )
-- import Interface( UserID(..), MsgQueueLen(..) )
import PoissonModel
import Random
import Signals
import GiaStation
-- import TreeCSMACD
-- import User

main :: IO ()
main = mainPoisson

-- mainPerf :: IO ()
-- mainPerf =
--        do
--        -- forM_ [0.85, 0.87, 0.9, 1.0] $ \lambda -> do
--        forM_ [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.62, 0.63, 0.65, 0.66, 0.67] $ \lambda -> do
--        -- forM_ [0.3, 0.31, 0.32, 0.33, 0.34] $ \lambda -> do
--        -- forM_ [0, 1 .. 9] $ \nsteps -> do
--         -- let y = lambda; lambda = 1.0
--         let y = lambda / fromIntegral nusers
--         -- nsteps <- read . head <$> getArgs
--         gens <- map split <$> replicateM nusers newStdGen
--         -- noiseGen <- randoms <$> newStdGen
--         noiseGen <- return $ repeat 0.0
--         let genBools = map rstreams gens
--             rstreams = randomBools y *** randomBools p
--             qs = 0.0
--             uids = UID <$> [1..]
--             -- params = zip uids (repeat ())
--             params = zip uids (repeat INF)
--             userParams = zip params genBools
--             -- userParams = [((UID 1, ()), ([True, False, False, False, True] ++ repeat False, [True ,  True , True , False, True ]))
--             --              ,((UID 2, ()), ([True] ++ repeat False, [True ,  False , False, False, False]))
--             --              -- ,((UID 3, ()), ([True] ++ repeat False, [False,  False, True ]))
--             --              ]
--             usrs = map initUser userParams
--             model = {- presentModel nsteps userParams $ -} runModel qs noiseGen nsteps usrs
--             mDelay = meanDelay $ model^.users
--             dlays = sum $ map (fromIntegral . view delays) $ model^.users :: Integer
--             nmsgs = sum $ map (fromIntegral . view numMsgs) $ model^.users :: Integer
--             ngene = sum $ map (fromIntegral . view numGenerated) $ model^.users :: Integer
--         void $ printf "%.2f\t%.5f\t%d\t%d\t%d\n" lambda (mDelay + 0.5 :: Double) dlays nmsgs ngene
--         -- print $ model^.stats
--         -- mapM_ print $ model^.users
--         -- putStrLn ""
--        putStrLn ""
--     where nusers = 100
--           nsteps = 1000000
--           p = 0.5 -- for Tree Algorithms

mainPoisson :: IO ()
mainPoisson = do
    let lambdas = [0.1, 0.2, 0.25, 0.3] ++ [0.31, 0.32 ..]
        snrs = [1000000, 8, 7] ++ [6.9, 6.8 .. 5.0]
    forM_ snrs $ \snr -> do
        putStrLn "#LAM       QS      SNR       MEAN     DELAYS      TRANS      GENER      NCONF"
        withBreak $ \break -> do
            forM_ lambdas $ \lambda -> do
            -- forM_ [1,2,3,4,5,6,7,8,9,10] $ \nConf -> do
                gen <- liftIO $ newStdGen -- bool transmission gen
                poissonGen <- liftIO $ newStdGen  -- poisson gen
                randGen <- liftIO $ randoms <$> newStdGen
                let numGen = poissonStream lambda poissonGen
                    -- lambda = 0.65 :: Double
                    -- numGen = repeat 1  -- one message generated each time
                    noiseGen = repeat 1.0
                    nsteps = 1000000 -- 10KK
                    baseSnr = fromDb snr
                    subSnr = 0.0
                    l = 424 -- #bits, dunno why
                    qs = probError baseSnr l

                    model = runPoissonModel nsteps $ initPoissonModel {-nConf-} numGen gen noiseGen baseSnr subSnr randGen
                    dlays = view delays model
                    ngene = view totalGenerated model
                    ntran = view transmitted model
                    nconf = view nConflicts model
                    -- nFalseConf = view (station . falseConflicts) model
                    -- lConf = view lenConf model

                    meanD = fromIntegral dlays / fromIntegral ntran :: Double
                liftIO $ void $ printf "%.2f %8.4f %8.2f %10.5f %10d %10d %10d %10d\n" lambda qs baseSnr (meanD - 0.5) dlays ntran ngene nconf
                -- void $ printf "%d\t%5.3f\t%5.3f\t%d\t%d\n" nConf (fromIntegral dlays / fromIntegral ntran :: Double)
                -- (fromIntegral lConf / fromIntegral nconf :: Double) lConf nconf
                when (meanD > 30.0) $ break ()
        putStrLn "\n\n"
  where withBreak = (`runContT` return) . callCC

    -- print $ model^.curConflictLen
    -- print $ model^.curConflictNum
    -- let Just (Undef lbl inp) = leftUndef . fromJust $ view (station . GiaStation.tree) model
    -- print $ all not $ tail lbl
    -- print inp
    -- mapM_ (print . cleanUser) $ model ^.. activeUsers.traversed

