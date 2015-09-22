{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module User
    ( User
    , UserParams
    , initUser
    , stepUserBefore
    , stepUserAfter
    , cleanUser
    , meanDelay
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict

import Interface( UserID
                , MsgQueueLen(..)
                , ForwMsg(..)
                , MsgResult(..)
                )
import Common( roll, append )

import qualified BasicTreeUser as ALG
    ( UserState
    , initState
    , willTransmitMsg
    , stepUserAfter
    , transmitMsg  -- for cleaning of generators
    )

type Delay = Int
type QMsg = (Delay, ForwMsg)
type MsgQueue = [QMsg]

data User = User {
        -- READ-ONLY DATA (TODO: RWS?)
        _userId      :: UserID,
        _msgQLen     :: MsgQueueLen,
        _msgQueue    :: MsgQueue,
        _generateMsg :: [Bool],
        _algState    :: ALG.UserState,
        _transmit    :: Bool,  -- is attempting to transmit
        _delays      :: Int,
        _numMsgs     :: Int
    } deriving Show

makeLenses ''User

type UserParams = ((UserID, MsgQueueLen), ([Bool], [Bool]))

initUser :: UserParams -> User
initUser ((uid, mqLen), (msgGen, transmitGen)) = User {
        _userId = uid,
        _msgQLen = mqLen,
        _msgQueue = [],
        _generateMsg = msgGen,
        _algState = ALG.initState transmitGen,
        _transmit = False,
        _delays = 0,
        _numMsgs = 0
    }

stepUserBefore :: State User (Maybe ForwMsg)
stepUserBefore = do
    mmsg <- maybeTransmit =<< use msgQueue
    maybeGenerateMsg
    return mmsg

maybeTransmit :: MsgQueue -> State User (Maybe ForwMsg)
maybeTransmit [] = transmit .= False >> return Nothing
maybeTransmit ((_delay, msg):_rest) = do
    willTransmit <- zoom algState ALG.willTransmitMsg
    transmit .= willTransmit
    tickDelays
    return $ if willTransmit then Just msg else Nothing

maybeGenerateMsg :: State User ()
maybeGenerateMsg = do
    qsize <- length <$> use msgQueue
    mqLen <- use msgQLen
    when (hasEmptySpace mqLen qsize) $ do
        willGenerate <- roll generateMsg
        when willGenerate $ do
            uid <- use userId
            append msgQueue $ newMsg uid

hasEmptySpace :: MsgQueueLen -> Int -> Bool
hasEmptySpace INF _ = True
hasEmptySpace (Bounded bound) qsize = qsize < bound

newMsg :: UserID -> QMsg
newMsg uid = (0, ForwMsg uid)

tickDelays :: State User ()
tickDelays = msgQueue.mapped._1 += 1

stepUserAfter :: MsgResult -> State User ()
stepUserAfter result = do
    wasTransmit <- use transmit
    zoom algState $ ALG.stepUserAfter result wasTransmit
    forceStats
    when (result == Success && wasTransmit) $ do
        (!cDelay, _msg) <- head <$> use msgQueue
        delays += cDelay
        numMsgs += 1
        msgQueue %= tail

forceStats :: State User ()
forceStats = do
    !_nmsgs  <- use numMsgs
    !_dlays  <- use delays
    return ()

cleanUser :: Int -> UserParams -> User -> User
cleanUser nsteps (_, (msgGen, transmitGen)) = (algState . ALG.transmitMsg .~ tG)
                                       . (generateMsg .~ mG)
    where tG = take nsteps transmitGen
          mG = take nsteps msgGen

meanDelay :: [User] -> Double
meanDelay us = if nmsgs /= 0 then dlays / nmsgs else 0
    where dlays = sum $ map (fromIntegral . view delays) us
          nmsgs = sum $ map (fromIntegral . view numMsgs) us
