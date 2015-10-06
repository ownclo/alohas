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
import Data.Maybe( fromJust )

import Interface( UserID
                , ForwMsg(..)
                , MsgResult(..)
                )
import Common( roll, isSuccess )
import qualified TwoBufferQueue as MSG

import qualified BasicTreeUser as ALG
    ( UserState
    , initState
    , willTransmitMsg
    , stepUserAfter
    , canShift
    , transmitMsg  -- for cleaning of generators
    )

type Delay = Int
type QMsg = (Delay, ForwMsg)
type MsgQueue = MSG.MsgSource QMsg
type MsgParams = MSG.MsgParams

data User = User {
        _userId      :: UserID,
        _msgQPrms    :: MsgParams,
        _msgQueue    :: MsgQueue,
        _generateMsg :: [Bool],
        _algState    :: ALG.UserState,
        _transmit    :: Bool,  -- is attempting to transmit
        _delays      :: Int,
        _numMsgs     :: Int
    } deriving Show

makeLenses ''User

type UserParams = ((UserID, MsgParams), ([Bool], [Bool]))

initUser :: UserParams -> User
initUser ((uid, mqLen), (msgGen, transmitGen)) = User {
        _userId = uid,
        _msgQPrms = mqLen,
        _msgQueue = MSG.init,
        _generateMsg = msgGen,
        _algState = ALG.initState transmitGen,
        _transmit = False,
        _delays = 0,
        _numMsgs = 0
    }

stepUserBefore :: State User (Maybe ForwMsg)
stepUserBefore = do
    mmsg <- maybeTransmit =<< use msgQueue
    zoom msgQueue $ MSG.tickDelays tick
    maybeGenerateMsg
    return mmsg

maybeTransmit :: MsgQueue -> State User (Maybe ForwMsg)
maybeTransmit mq = case MSG.getTransmit mq of
    Nothing -> transmit .= False >> return Nothing
    Just (_delay, msg) -> do
        willTransmit <- zoom algState ALG.willTransmitMsg
        transmit .= willTransmit
        return $ if willTransmit then Just msg else Nothing

maybeGenerateMsg :: State User ()
maybeGenerateMsg = do
    mq <- use msgQueue
    mqPrms <- use msgQPrms
    when (MSG.canGenerate mqPrms mq) $ do
        willGenerate <- roll generateMsg
        when willGenerate $ do
            msg <- newMsg <$> use userId
            zoom msgQueue $ MSG.storeGenerate msg

newMsg :: UserID -> QMsg
newMsg uid = (0, ForwMsg uid)

tick :: QMsg -> QMsg
tick = _1 +~ 1

stepUserAfter :: MsgResult -> State User ()
stepUserAfter result = do
    wasTransmit <- use transmit
    zoom algState $ ALG.stepUserAfter result wasTransmit
    forceStats
    canShift <- zoom algState $ ALG.canShift MSG.sourceType result wasTransmit
    when (isSuccess result && wasTransmit) $ do
        (!cDelay, _msg) <- fromJust . MSG.getTransmit <$> use msgQueue
        delays += cDelay
        numMsgs += 1
        zoom msgQueue MSG.dropMsg
    -- XXX: SHIFT FOR TBQ IS NOT PERFORMED UNTIL CONFLICT IS OVER,
    -- whereas for ordinary queue a shift is performed immediately
    -- after successful transmission.
    when canShift $ zoom msgQueue MSG.shiftTransmit

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
