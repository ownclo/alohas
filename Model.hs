{-# LANGUAGE TemplateHaskell #-}
module Model where

import Control.Monad.State
import Control.Applicative
import Control.Lens

data ModelState = ModelState {
        _forwChannel :: ForwChannel,
        _backChannel :: BackChannel,
        _station :: Station,
        _users :: [User],
        _stats :: Stats
    } deriving Show

data ForwChannel = ForwChannel
  deriving Show

data BackChannel = BackChannel
  deriving Show

data Station = Station
  deriving Show

data User = User
  deriving Show

data Stats = Stats
  deriving Show

data ForwMsg = ForwMsg
  deriving Show

makeLenses ''ModelState

type Model = State ModelState

initModel :: ModelState
initModel = ModelState {
        _forwChannel = ForwChannel,
        _backChannel = BackChannel,
        _station = Station,
        _users = [],
        _stats = Stats
    }

runModel :: Int -> ModelState
runModel num = execState steps initModel
    where steps = replicateM_ num stepModel

stepModel :: Model ()
stepModel = do
    msgs <- stepUsersBefore
    stepForwChannel
    stepStation
    stepBackChannel
    stepUsersAfter

stepUsersBefore :: Model [ForwMsg]
stepUsersBefore = zoom (users.traversed) stepUserBefore

stepUserBefore :: State User [ForwMsg]
stepUserBefore = return [ForwMsg]

stepForwChannel :: Model ()
stepForwChannel = undefined

stepStation :: Model ()
stepStation = undefined

stepBackChannel :: Model ()
stepBackChannel = undefined

stepUsersAfter :: Model ()
stepUsersAfter = undefined

main :: IO ()
main = print $ runModel 1