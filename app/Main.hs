module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
-- import Control.Arrow( (***) )
import Control.Lens
-- import Data.Maybe( fromJust )

import System.Environment( getArgs )
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

mainPoisson :: IO ()
mainPoisson = do
    n : sub : args <- getArgs
    let
        noiseR = read n
        subSnr = read sub
        lambdas = [0.1, 0.11 ..]
        -- lambdas = [0.1, 0.2, 0.25, 0.3] ++ [0.31, 0.32 ..]
        -- snrs = [1000000, 8, 7] ++ [6.9, 6.8 .. 5.0]
        -- subSnrs = [0.1, 0.2 .. 3.0]
        subSnrs = [subSnr]
        baseSnr = fromDb 10
    forM_ subSnrs $ \subSnr -> do
        putStrLn "#SSNR LAM       QS      SNR       MEAN     DELAYS      TRANS      GENER      NCONF"
        withBreak $ \break -> do
            forM_ lambdas $ \lambda -> do
            -- forM_ [1,2,3,4,5,6,7,8,9,10] $ \nConf -> do
                gen <- liftIO $ newStdGen -- bool transmission gen
                poissonGen <- liftIO $ newStdGen  -- poisson gen
                randGen <- liftIO $ randoms <$> newStdGen
                let numGen = poissonStream lambda poissonGen
                    -- lambda = 0.65 :: Double
                    -- numGen = repeat 1  -- one message generated each time
                    noiseGen = repeat noiseR
                    nsteps = 1000000 -- 10KK
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
                liftIO $ void $ printf "%4.2f %.2f %8.4f %8.2f %10.5f %10d %10d %10d %10d\n" subSnr lambda qs baseSnr (meanD - 0.5) dlays ntran ngene nconf
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

