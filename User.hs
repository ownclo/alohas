{-# LANGUAGE TemplateHaskell #-}

module User
    ( User
    , UserParams
    , initUser
    , stepUserBefore
    , stepUserAfter
    , cleanUser
    ) where

import Control.Lens
import Control.Monad.State

import Interface( ForwMsg(..), MsgResult(..) )
import Common( roll, append )

data User = User {
        _msgQueue    :: [ForwMsg],
        _generateMsg :: [Bool],
        _transmitMsg :: [Bool],
        _transmit    :: Bool  -- is attempting to transmit
    } deriving Show

makeLenses ''User

type UserParams = ([Bool], [Bool])

initUser :: UserParams -> User
initUser (msgGen, transmitGen) = User {
        _msgQueue = [],
        _generateMsg = msgGen,
        _transmitMsg = transmitGen,
        _transmit = False
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
        return $ Just msg
    else return Nothing

maybeTransmitMsg :: State User Bool
maybeTransmitMsg = roll transmitMsg

stepUserAfter :: MsgResult -> State User ()
stepUserAfter Empty = return ()
stepUserAfter Conflict = return ()
stepUserAfter Success = do
        wasTransmit <- use transmit
        when wasTransmit $ msgQueue %= tail

cleanUser :: Int -> UserParams -> User -> User
cleanUser nsteps (msgGen, transmitGen) = (transmitMsg .~ tG)
                                       . (generateMsg .~ mG)
    where tG = take nsteps transmitGen
          mG = take nsteps msgGen
