{-# LANGUAGE TemplateHaskell #-}

module BasicTreeUser where

import Control.Lens
import Control.Monad.State.Strict

import Common( roll, isSuccess, isConflict )
import Interface( MsgResult(..), MsgSourceType(..) )

import TreeCSMACD

data UserState = UserState {
         _transmitMsg :: [Bool],
         _label :: Label,
         _tree  :: Maybe (Tree ())
    } deriving Show

makeLenses ''UserState

initState :: [Bool] -> UserState
initState gen = UserState {
        _transmitMsg = gen,
        _label = initLabel,
        _tree  = initTree
    }

cleanState :: UserState -> UserState
cleanState (UserState _gen lbl t) = UserState [] lbl t

willTransmitMsg :: State UserState Bool
willTransmitMsg = do
    lbl <- use label
    ctr <- use tree
    label %= updateLabel ctr
    return $ canTransmit lbl ctr

stepUserAfter :: MsgResult -> Bool -> State UserState ()
stepUserAfter res wasTransmit = do
    tree %= updateTreeS res
    when (isConflict res && wasTransmit) $ do
        transmitNow <- roll transmitMsg
        label %= decideTransmit transmitNow

-- XXX: in the end of a conflict, no user has something in
-- its OUT buffer, so shift in the end SHOULD be safe
canShift :: MsgSourceType -> MsgResult -> Bool -> State UserState Bool
canShift BoundedQueue res wasTransmit = return $ isSuccess res && wasTransmit
canShift TwoBufferQueue _res _wasTransmit = uses tree isResolved
