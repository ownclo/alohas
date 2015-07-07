module Model where

import Control.Monad.State

data ModelState = ModelState {
        channel :: Channel,
        station :: Station,
        users :: [User],
        stats :: Stats
    } deriving Show

data Channel = Channel
  deriving Show

data Station = Station
  deriving Show

data User = User
  deriving Show

data Stats = Stats
  deriving Show

type Model = State ModelState ()

initModel :: ModelState
initModel = ModelState {
        channel = Channel,
        station = Station,
        users = [],
        stats = Stats
    }

runModel :: Model -> ModelState -> ModelState
runModel = execState

stepModel :: Model -> Model
stepModel = undefined

main :: IO ()
main = print $ flip runModel initModel $ return ()