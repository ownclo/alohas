{-# LANGUAGE TemplateHaskell #-}

module AlohaUser where

import Control.Lens
import Control.Monad.State.Strict

import Common( roll )
import Interface( MsgResult(..) )

data UserState = UserState {
         _transmitMsg :: [Bool]
    } deriving Show

makeLenses ''UserState

initState :: [Bool] -> UserState
initState = UserState

willTransmitMsg :: State UserState Bool
willTransmitMsg = roll transmitMsg

stepUserAfter :: MsgResult -> Bool -> State UserState ()
stepUserAfter _ _ = return ()
