{-# LANGUAGE TemplateHaskell #-}

module AlohaUser where

import Control.Lens
import Control.Monad.State.Strict

import Common( roll )
import Interface( MsgResult(..), MsgSourceType(..) )

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

canShift :: MsgSourceType -> MsgResult -> Bool -> State UserState Bool
canShift _ res wasTransmit = return $ res == Success && wasTransmit
