{-# LANGUAGE TemplateHaskell #-}

module NoisyChannel
    ( stepChannel
    , Channel
    , initChannel
    ) where

import Control.Applicative( (<$>) )
import Control.Lens
import Control.Monad.State.Strict

import Interface( ForwMsg
                , ForwSignal(..)
                )
import Common( roll )

data Channel = Channel {
        _generateNoise :: [Double]
    } deriving Show

makeLenses ''Channel

initChannel :: [Double] -> Channel
initChannel = Channel

stepChannel :: [ForwMsg] -> State Channel ForwSignal
stepChannel signals = ForwSignal signals <$> roll generateNoise
