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

initModel :: Double -> [Double] -> [User] -> ModelState
initModel qs noiseGen usrs = ModelState {
        _forwChannel = CH.initChannel noiseGen,
        _backChannel = BackChannel,
        _station = ST.initStation qs,
        _users = usrs,
        _stats = Stats []
    }

runModel :: Double -> [Double] -> Int -> [User] -> ModelState
runModel qs noiseGen nsteps usrs = execState steps $ initModel qs noiseGen usrs
    where steps = replicateM_ nsteps stepModel

presentModel :: Int -> [UserParams] -> ModelState -> ModelState
presentModel nsteps userParams model = model & users .~ newUsers
    where newUsers = map cleanUser' uparams
          uparams = zip userParams usrs
          usrs = model ^. users
          cleanUser' = uncurry $ cleanUser nsteps

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
stepStation = zoom station . ST.stepStation

stepStatistics :: StationFeedback -> Model StationFeedback
stepStatistics = zoom stats . stepStats
    where stepStats res = append history res  -- prepend history res
                       >> return res

stepBackChannel :: StationFeedback -> Model StationFeedback
stepBackChannel = zoom backChannel . return

stepUsersAfter :: StationFeedback -> Model ()
stepUsersAfter = zoom (users.traversed) . stepUserAfter
