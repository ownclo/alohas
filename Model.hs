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

data User = User {
        _msgQueue    :: [ForwMsg],
        _generateMsg :: [Bool],
        _transmit    :: Bool  -- is attempting to transmit
    } deriving Show

data Stats = Stats {
        _history :: [MsgResult]
    } deriving Show

data ForwMsg = ForwMsg
  deriving Show

data MsgResult = Empty
               | Success
               | Conflict
               deriving Show

makeLenses ''User
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

initUser :: [Bool] -> User
initUser msgGen = User {
        _msgQueue = [],
        _generateMsg = msgGen,
        _transmit = False
    }

runModel :: Int -> Int -> ModelState
runModel nsteps nusers = execState steps $ initModel nusers
    where steps = replicateM_ nsteps stepModel

-- TODO: get the initial generator; trim to nsteps; replace generateMsg
showModel :: ModelState -> ModelState
showModel = users.traversed.generateMsg .~ []

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

stepUserBefore :: State User (Maybe ForwMsg)
stepUserBefore = do
    msgQ <- use msgQueue
    mmsg <- case msgQ of
              [] -> maybeGenerateMsg
              [msg] -> return $ Just msg
              _ -> error "msgQueue size greater than one"
    willTransmit <- case mmsg of
                      Nothing -> return False
                      Just _ -> maybeTransmitMsg
    transmit .= willTransmit
    return $ if willTransmit then mmsg else Nothing


maybeGenerateMsg :: State User (Maybe ForwMsg)
maybeGenerateMsg = do
    willGenerate <- roll generateMsg
    if willGenerate then do
        let msg = ForwMsg
        append msgQueue msg
        return $ Just msg
    else return Nothing

roll :: MonadState s m => Lens' s [a] -> m a
roll alens = do
    x:xs <- use alens
    alens .= xs
    return x

append :: MonadState s m => Lens' s [a] -> a -> m ()
append alens val = alens <>= [val]

maybeTransmitMsg :: State User Bool
maybeTransmitMsg = return True

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

stepUserAfter :: MsgResult -> State User ()
stepUserAfter Empty = return ()
stepUserAfter Conflict = return ()
stepUserAfter Success = do
        wasTransmit <- use transmit
        when wasTransmit $ msgQueue %= tail

main :: IO ()
main = print . showModel $ runModel 2 2
