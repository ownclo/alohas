{-# LANGUAGE TemplateHaskell #-}

module BasicTreeUser where

import Control.Lens
import Control.Monad.State.Strict

import Common( roll )
import Interface( MsgResult(..) )

import TreeCSMACD

data UserState = UserState {
         _transmitMsg :: [Bool],
         _label :: Label,
         _tree  :: Maybe CTree
    } deriving Show

makeLenses ''UserState

initState :: [Bool] -> UserState
initState gen = UserState {
        _transmitMsg = gen,
        _label = initLabel,
        _tree  = initCTree
    }

willTransmitMsg :: State UserState Bool
willTransmitMsg = do
    lbl <- use label
    ctr <- use tree
    return $ canTransmit lbl ctr

stepUserAfter :: MsgResult -> Bool -> State UserState ()
stepUserAfter res wasTransmit = do
    tree %= updateCTree res
    when (res == Conflict && wasTransmit) $ do
        transmitNow <- roll transmitMsg
        Just ctr <- use tree
        label .= decideTransmit transmitNow ctr
