{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Monad.State
import Control.Applicative
import Control.Lens

import Data.Maybe( maybeToList )

import User( User, initUser, stepUserBefore, stepUserAfter, cleanUser )
import Interface( ForwMsg(..), MsgResult(..) )
import Common( append )

data ModelState = ModelState {
        _forwChannel :: ForwChannel,
        _backChannel :: BackChannel,
        _station     :: Station,
        _users       :: [User],
        _stats       :: Stats
    } deriving Show

data ForwChannel = ForwChannel
  deriving Show

data BackChannel = BackChannel
  deriving Show

data Station = Station
  deriving Show

data Stats = Stats {
        _history :: [MsgResult]
    } deriving Show

makeLenses ''Stats
makeLenses ''ModelState

type Model = State ModelState

initModel :: Int -> ModelState
initModel n = ModelState {
        _forwChannel = ForwChannel,
        _backChannel = BackChannel,
        _station = Station,
        _users = replicate n $ initUser (repeat True),
        _stats = Stats []
    }

runModel :: Int -> Int -> ModelState
runModel nsteps nusers = execState steps $ initModel nusers
    where steps = replicateM_ nsteps stepModel

-- TODO: get the initial generator; trim to nsteps; replace generateMsg
showModel :: ModelState -> ModelState
showModel = users.traversed %~ cleanUser

stepModel :: Model ()
stepModel =
        stepUsersBefore
    >>= stepForwChannel
    >>= stepStation
    >>= stepStatistics
    >>= stepBackChannel
    >>= stepUsersAfter

stepUsersBefore :: Model [ForwMsg]
stepUsersBefore = zoom (users.traversed) $ maybeToList <$> stepUserBefore

stepForwChannel :: [ForwMsg] -> Model [ForwMsg]
stepForwChannel = zoom forwChannel . return

stepStation :: [ForwMsg] -> Model MsgResult
stepStation = zoom station . stepStation'

stepStation' :: [ForwMsg] -> State Station MsgResult
stepStation' = return . recvMsgs

recvMsgs :: [ForwMsg] -> MsgResult
recvMsgs []  = Empty
recvMsgs [_] = Success
recvMsgs _   = Conflict

stepStatistics :: MsgResult -> Model MsgResult
stepStatistics = zoom stats . stepStats
    where stepStats res = append history res  -- history %= (res:)
                       >> return res

stepBackChannel :: MsgResult -> Model MsgResult
stepBackChannel = zoom backChannel . return

stepUsersAfter :: MsgResult -> Model ()
stepUsersAfter = zoom (users.traversed) . stepUserAfter

main :: IO ()
main = print . showModel $ runModel 2 2
