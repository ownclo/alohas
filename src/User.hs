{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module User
    ( User
    , UserParams
    , initUser
    , stepUserBefore
    , stepUserAfter
    , cleanUser
    , meanDelay
    , delays
    , numMsgs
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Maybe( fromJust )

import Interface( UserID
                , ForwMsg(..)
                , StationFeedback
                )
import Common( roll, isSuccess )
-- import qualified TwoBufferQueue as MSG
import qualified BoundedQueue as MSG

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
        _userId        :: UserID,
        _msgQPrms      :: MsgParams,
        _msgQueue      :: MsgQueue,
        _generateMsg   :: [Bool],
        _algState      :: ALG.UserState,
        _transmit      :: Bool,  -- is attempting to transmit
        _delays        :: Int,
        _numMsgs       :: Int,
        _realWindow    :: Bool,
        _reconstructed :: Bool
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
        _numMsgs = 0,
        _realWindow = True,
        _reconstructed = False
    }

stepUserBefore :: State User (Maybe ForwMsg)
stepUserBefore = do
    mmsg <- maybeTransmit =<< use msgQueue
    willTick <- use realWindow
    when willTick $ zoom msgQueue $ MSG.tickDelays tick
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
    -- XXX: we can not allow a message to be generated in a skipped window.
    aRealWindow <- use realWindow
    when (MSG.canGenerate mqPrms mq && aRealWindow) $ do
        willGenerate <- roll generateMsg
        when willGenerate $ do
            msg <- uses userId newMsg
            zoom msgQueue $ MSG.storeGenerate msg

newMsg :: UserID -> QMsg
newMsg uid = (0, ForwMsg uid)

tick :: QMsg -> QMsg
tick = _1 +~ 1

stepUserAfter :: StationFeedback -> State User ()
stepUserAfter (result, transmittedWindow, mReconstructed) = do
    wasTransmit <- use transmit
    rec <- use reconstructed
    realWindow .= transmittedWindow
    zoom algState $ ALG.stepUserAfter result wasTransmit
    forceStats

    -- XXX: generate need to be called before BoundedQueue.dropMsg,
    -- because doing otherwise would allow a user to transmit & generate
    -- a message in an empty queue in a single round.
    -- For 2BQ that is not the case, 
    maybeGenerateMsg

    when (isSuccess result && wasTransmit) $ do
        -- If the message was already reconstructed, does not agregate
        -- message delay statistics, but drop the flag back to False.
        if not rec then saveMsgDelay else reconstructed .= False
        zoom msgQueue MSG.dropMsg

    myUserId <- use userId
    when (msgReconstructed myUserId mReconstructed) $ do
        saveMsgDelay
        reconstructed .= True

    -- XXX: SHIFT FOR TBQ IS NOT PERFORMED UNTIL CONFLICT IS OVER,
    -- whereas for ordinary queue a shift is performed immediately
    -- after successful transmission.
    canShift <- zoom algState $ ALG.canShift MSG.sourceType result wasTransmit
    when canShift $ do
        -- XXX: when there is time for shifting (after the end of conflict
        -- for TBQ or after 'virtual' or 'real' success for BoundedQueue,
        -- even if conflict ended with 'virtual' window, one needs to tick
        -- delays.
        realWindow .= True
        zoom msgQueue MSG.shiftTransmit

msgReconstructed :: UserID -> Maybe UserID -> Bool
msgReconstructed _ Nothing = False
msgReconstructed myUid (Just reconstructedUid) = reconstructedUid == myUid

saveMsgDelay :: State User ()
saveMsgDelay = do
    (!cDelay, _msg) <- fromJust . MSG.getTransmit <$> use msgQueue
    delays += cDelay
    numMsgs += 1

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
