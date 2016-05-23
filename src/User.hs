{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module User
    ( User
    , UserParams
    , initUser
    , cleanUser
    , stepUserBefore
    , stepUserAfter
    , delay
    ) where

import Control.Lens
import Control.Monad.State.Strict

import Interface( UserID
                , ForwMsg(..)
                , StationFeedback
                )
import Common( isMySuccess )
-- import qualified TwoBufferQueue as MSG
-- import qualified BoundedQueue as MSG

import qualified BasicTreeUser as ALG
    ( UserState
    , initState
    , cleanState
    , willTransmitMsg
    , stepUserAfter
    )

type Delay = Int
type QMsg = (Delay, ForwMsg)

data User = User {
        _message    :: QMsg,
        _algState   :: ALG.UserState,
        _transmit   :: Bool,  -- is attempting to transmit
        _delay      :: Int,
        _realWindow :: Bool,
        _finished   :: Bool
    } deriving Show

makeLenses ''User

type UserParams = (QMsg, [Bool])

initUser :: UserParams -> User
initUser (qmsg, transmitGen) = User {
        _message = qmsg,
        _algState = ALG.initState transmitGen,
        _transmit = False,
        _delay = 0,
        _realWindow = True,
        _finished = False
    }

cleanUser :: User -> User
cleanUser = over algState ALG.cleanState

stepUserBefore :: State User (Maybe ForwMsg)
stepUserBefore = do
    fin <- use finished
    if not fin then maybeTransmit
    else return Nothing

maybeTransmit :: State User (Maybe ForwMsg)
maybeTransmit = do
    willTransmit <- zoom algState ALG.willTransmitMsg
    transmit .= willTransmit
    (_delay, forwMsg) <- use message
    return $ if willTransmit then Just forwMsg
             else Nothing

tick :: QMsg -> QMsg
tick = _1 +~ 1

stepUserAfter :: StationFeedback -> State User ()
stepUserAfter (result, transmittedWindow, mReconstructed) = do
    (_, ForwMsg myUserId) <- use message
    wasTransmit <- use transmit

    realWindow .= transmittedWindow
    willTick <- use realWindow
    when willTick $ message %= tick

    zoom algState $ ALG.stepUserAfter result wasTransmit
    forceStats

    let isReconstructed = msgReconstructed myUserId mReconstructed
        mySuccess = isMySuccess result myUserId

    when (mySuccess || isReconstructed) saveMsgDelay


msgReconstructed :: UserID -> Maybe UserID -> Bool
msgReconstructed _ Nothing = False
msgReconstructed myUid (Just reconstructedUid) = reconstructedUid == myUid

-- Save get triggered only once
saveMsgDelay :: State User ()
saveMsgDelay = do
    fin <- use finished
    unless fin $ do
        (!cDelay, _msg) <- use message
        finished .= True
        delay .= cDelay

forceStats :: State User ()
forceStats = do
    !_dlay  <- use delay
    return ()
