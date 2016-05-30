{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Lens

import Data.Maybe( maybeToList )

import Interface( ForwMsg
                , ForwSignal
                , StationFeedback
                )
import Common( append )
import User

import qualified GiaStation as ST
-- import qualified NoMemStation as ST

import qualified NoisyChannel as CH
-- import qualified NoiselessChannel as CH

data ModelState = ModelState {
        _forwChannel :: CH.Channel,
        _backChannel :: BackChannel,
        _station     :: ST.Station,
        _users       :: [User],
        _stats       :: Stats
    } deriving Show

data BackChannel = BackChannel
  deriving Show

data Stats = Stats {
        _history :: [StationFeedback]
    } deriving Show

makeLenses ''Stats
makeLenses ''ModelState

type Model = State ModelState

initModel :: Double -> Double -> [Double] -> [Double] -> [User] -> ModelState
initModel baseSnr subSnr gen noiseGen usrs = ModelState {
        _forwChannel = CH.initChannel noiseGen,
        _backChannel = BackChannel,
        _station = ST.initStation baseSnr subSnr gen,
        _users = usrs,
        _stats = Stats []
    }

runModel :: Double -> Double -> [Double] -> [Double] -> Int -> [User] -> ModelState
runModel baseSnr subSnr gen noiseGen nsteps usrs = execState steps $ initModel baseSnr subSnr gen noiseGen usrs
    where steps = replicateM_ nsteps stepModel

stepModel :: Model ()
stepModel = stepUsersBefore
        >>= stepForwChannel
        >>= stepStation
        -- >>= stepStatistics
        >>= stepBackChannel
        >>= stepUsersAfter

stepUsersBefore :: Model [ForwMsg]
stepUsersBefore = zoom (users.traversed) $ maybeToList <$> stepUserBefore

stepForwChannel :: [ForwMsg] -> Model ForwSignal
stepForwChannel = zoom forwChannel . CH.stepChannel

stepStation :: ForwSignal -> Model StationFeedback
stepStation sig = do
    (_, feedBack) <- zoom station $ ST.stepStation sig
    return feedBack

stepStatistics :: StationFeedback -> Model StationFeedback
stepStatistics = zoom stats . stepStats
    where stepStats res = append history res  -- prepend history res
                       >> return res

stepBackChannel :: StationFeedback -> Model StationFeedback
stepBackChannel = zoom backChannel . return

stepUsersAfter :: StationFeedback -> Model ()
stepUsersAfter = zoom (users.traversed) . stepUserAfter
