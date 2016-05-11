module NoiselessChannel
    ( stepChannel
    , Channel
    , initChannel
    ) where

import Control.Monad.State.Strict

import Interface( ForwMsg
                , ForwSignal(..)
                )

data Channel = Channel deriving Show

initChannel :: Channel
initChannel = Channel

stepChannel :: [ForwMsg] -> State Channel ForwSignal
stepChannel signals = return $ ForwSignal signals 0.0
