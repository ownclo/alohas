{-# LANGUAGE TemplateHaskell #-}

module User
    ( User
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
        _transmit    :: Bool  -- is attempting to transmit
    } deriving Show

makeLenses ''User

initUser :: [Bool] -> User
initUser msgGen = User {
        _msgQueue = [],
        _generateMsg = msgGen,
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
maybeTransmitMsg = return True

stepUserAfter :: MsgResult -> State User ()
stepUserAfter Empty = return ()
stepUserAfter Conflict = return ()
stepUserAfter Success = do
        wasTransmit <- use transmit
        when wasTransmit $ msgQueue %= tail

-- TODO: get the initial generator; trim to nsteps; replace generateMsg
cleanUser :: User -> User
cleanUser = generateMsg .~ []
