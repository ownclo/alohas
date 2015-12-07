{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GiaStation
    ( stepStation
    , Station
    , initStation
    ) where


import Control.Lens
import Control.Monad.State.Strict
import Data.List( (\\) )

import Interface( ForwMsg(..)
                -- , UserID
                , StationFeedback
                , MsgResult(..)
                )
import Common( prepend )

type StationInput = [ForwMsg]
type StationHistory = [StationInput]

data Station = Station {
        _history :: StationHistory
    } deriving Show

makeLenses ''Station

initStation :: Station
initStation = Station []

stepStation :: StationInput -> State Station StationFeedback
stepStation input = do
        let msgResult = recvMsgs input
        history %= cancelInterferenceOn input
        recovered <- uses history recoveredSignals
        prepend history input
        h <- use history
        return (msgResult, map (map _uid) h)

recvMsgs :: StationInput -> MsgResult
recvMsgs []  = Empty
recvMsgs [ForwMsg uid] = Success uid
recvMsgs ls   = Conflict $ map _uid ls

cancelInterferenceOn :: StationInput -> StationHistory -> StationHistory
cancelInterferenceOn input = map $ subtractSignals input

recoveredSignals :: StationHistory -> [ForwMsg]
recoveredSignals = concat . filter ((1==) . length)

subtractSignals :: StationInput -> StationInput -> StationInput
subtractSignals = flip (\\)
