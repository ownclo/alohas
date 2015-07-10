{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Model where

import Control.Monad.State
import Control.Applicative
import Control.Lens

import Data.Maybe( maybeToList )

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

data User = User {
        _msgQueue :: [ForwMsg],
        _generateMsg :: [Bool]
    } deriving Show

data Stats = Stats
  deriving Show

data ForwMsg = ForwMsg
  deriving Show

makeLenses ''User
makeLenses ''ModelState

type Model = State ModelState

initModel :: Int -> ModelState
initModel n = ModelState {
        _forwChannel = ForwChannel,
        _backChannel = BackChannel,
        _station = Station,
        _users = replicate n $ initUser (repeat True),
        _stats = Stats
    }

initUser :: [Bool] -> User
initUser msgGen = User {
        _msgQueue = [],
        _generateMsg = msgGen
    }

runModel :: Int -> Int -> ModelState
runModel nsteps nusers = execState steps $ initModel nusers
    where steps = replicateM_ nsteps stepModel

stepModel :: Model ()
stepModel = do
    msgs <- stepUsersBefore
    stepForwChannel
    stepStation
    stepBackChannel
    stepUsersAfter

stepUsersBefore :: Model [ForwMsg]
stepUsersBefore = zoom (users.traversed) $ maybeToList <$> stepUserBefore

stepUserBefore :: State User (Maybe ForwMsg)
stepUserBefore = do
    msgQ <- use msgQueue
    mmsg <- case msgQ of
              [] -> maybeGenerateMsg
              [msg] -> return $ Just msg
              _ -> error "msgQueue size greater than one"
    willTransmit <- case mmsg of
                      Nothing -> return False
                      Just msg -> maybeTransmitMsg
    return $ if willTransmit then mmsg else Nothing


maybeGenerateMsg :: State User (Maybe ForwMsg)
maybeGenerateMsg = do
    willGenerate <- roll generateMsg
    return $ if willGenerate then Just ForwMsg else Nothing

roll :: MonadState s m => Lens' s [a] -> m a
roll lens = do
    x:xs <- use lens
    lens .= xs
    return x

maybeTransmitMsg :: State User Bool
maybeTransmitMsg = return True

stepForwChannel :: Model ()
stepForwChannel = undefined

stepStation :: Model ()
stepStation = undefined

stepBackChannel :: Model ()
stepBackChannel = undefined

stepUsersAfter :: Model ()
stepUsersAfter = undefined

main :: IO ()
main = print $ runModel 1 1