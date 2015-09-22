{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module TwoBufferQueue where

import Control.Monad.State.Strict
import Data.Maybe( isNothing )

type MsgParams = ()
type MsgSource a = TBQ a

data TBQ a = TBQ {
           _in  :: Maybe a,
           _out :: Maybe a
       } deriving (Show, Eq)

instance Functor TBQ where
        fmap f (TBQ i o) = TBQ (fmap f i) (fmap f o)

init :: TBQ a
init = TBQ Nothing Nothing

canGenerate :: MsgParams -> TBQ a -> Bool
canGenerate () = isNothing . _in

storeGenerate' :: a -> TBQ a -> TBQ a
storeGenerate' i' (TBQ Nothing o) = TBQ (Just i') o
storeGenerate' _ _ = error "Called storeGenerate' with non-empty IN cell"

storeGenerate :: a -> State (TBQ a) ()
storeGenerate = modify . storeGenerate'

getTransmit :: TBQ a -> Maybe a
getTransmit = _out

shiftTransmit' :: TBQ a -> TBQ a
shiftTransmit' (TBQ i _o) = TBQ Nothing i

shiftTransmit :: State (TBQ a) ()
shiftTransmit = modify shiftTransmit'

tickDelays :: (a -> a) -> State (TBQ a) ()
tickDelays tick = modify $ fmap tick
