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
        _numMsgs   :: Int,
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
        curDelay .= 0
        return $ Just msg
    else return Nothing

maybeTransmitMsg :: State User Bool
maybeTransmitMsg = roll transmitMsg

stepUserAfter :: MsgResult -> State User ()
stepUserAfter result = do
    curDelay += 1
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

meanDelay :: User -> Double
meanDelay User{ _numMsgs = 0 } = 0
meanDelay u = dlays / nmsgs
    where dlays = fromIntegral $ view delays u
          nmsgs = fromIntegral $ view numMsgs u
