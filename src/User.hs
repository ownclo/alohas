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

import Interface( UserID, ForwMsg(..), MsgResult(..) )
import Common( roll, append )

import qualified BasicTreeUser as ALG
    ( UserState
    , initState
    , willTransmitMsg
    , stepUserAfter
    , transmitMsg  -- for cleaning of generators
    )

data User = User {
        _userId      :: UserID,
        _msgQueue    :: [ForwMsg],
        _generateMsg :: [Bool],
        _algState    :: ALG.UserState,
        _transmit    :: Bool,  -- is attempting to transmit
        _delays      :: Int,
        _numMsgs     :: Int,
        _curDelay    :: Int
    } deriving Show

makeLenses ''User

type UserParams = (UserID, ([Bool], [Bool]))

initUser :: UserParams -> User
initUser (uid, (msgGen, transmitGen)) = User {
        _userId = uid,
        _msgQueue = [],
        _generateMsg = msgGen,
        _algState = ALG.initState transmitGen,
        _transmit = False,
        _delays = 0,
        _numMsgs = 0,
        _curDelay = 0
    }

stepUserBefore :: State User (Maybe ForwMsg)
stepUserBefore = do
    mmsg <- maybeTransmit =<< use msgQueue
    maybeGenerateMsg
    return mmsg

maybeTransmit :: [ForwMsg] -> State User (Maybe ForwMsg)
maybeTransmit [] = transmit .= False >> return Nothing
maybeTransmit [msg] = do
    willTransmit <- zoom algState ALG.willTransmitMsg
    transmit .= willTransmit
    curDelay += 1
    return $ if willTransmit then Just msg else Nothing
maybeTransmit _ = error "msgQueue size greater than one"

maybeGenerateMsg :: State User ()
maybeGenerateMsg = do
    qsize <- length <$> use msgQueue
    when (qsize < 1) $ do
        willGenerate <- roll generateMsg
        when willGenerate $ do
            uid <- use userId
            append msgQueue $ ForwMsg uid
            curDelay .= 0

stepUserAfter :: MsgResult -> State User ()
stepUserAfter result = do
    wasTransmit <- use transmit
    zoom algState $ ALG.stepUserAfter result wasTransmit
    forceStats
    when (result == Success && wasTransmit) $ do
        cDelay <- use curDelay
        delays += cDelay
        numMsgs += 1
        msgQueue %= tail

forceStats :: State User ()
forceStats = do
    !_cDelay <- use curDelay
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
