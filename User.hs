{-# LANGUAGE TemplateHaskell #-}

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

import Interface( ForwMsg(..), MsgResult(..) )
import Common( roll, append )

data User = User {
        _msgQueue    :: [ForwMsg],
        _generateMsg :: [Bool],
        _transmitMsg :: [Bool],
        _transmit    :: Bool,  -- is attempting to transmit
        _delays      :: Int,
        _numMsgs     :: Int,
        _curDelay    :: Int
    } deriving Show

makeLenses ''User

type UserParams = ([Bool], [Bool])

initUser :: UserParams -> User
initUser (msgGen, transmitGen) = User {
        _msgQueue = [],
        _generateMsg = msgGen,
        _transmitMsg = transmitGen,
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
    willTransmit <- maybeTransmitMsg
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
            append msgQueue ForwMsg
            curDelay .= 0

maybeTransmitMsg :: State User Bool
maybeTransmitMsg = roll transmitMsg

stepUserAfter :: MsgResult -> State User ()
stepUserAfter result = do
    wasTransmit <- use transmit
    when (result == Success && wasTransmit) $ do
        cDelay <- use curDelay
        delays += cDelay
        numMsgs += 1
        msgQueue %= tail

cleanUser :: Int -> UserParams -> User -> User
cleanUser nsteps (msgGen, transmitGen) = (transmitMsg .~ tG)
                                       . (generateMsg .~ mG)
    where tG = take nsteps transmitGen
          mG = take nsteps msgGen

meanDelay :: [User] -> Double
meanDelay us = if nmsgs /= 0 then dlays / nmsgs else 0
    where dlays = sum $ map (fromIntegral . view delays) us
          nmsgs = sum $ map (fromIntegral . view numMsgs) us
