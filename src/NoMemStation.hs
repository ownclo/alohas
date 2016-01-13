{-# LANGUAGE TupleSections #-}

module NoMemStation
    ( stepStation
    , Station
    , initStation
    ) where


-- mport Control.Lens
import Control.Monad.State.Strict

import Interface( ForwMsg(..)
                -- , UserID
                , StationFeedback
                , MsgResult(..)
                )

data Station = Station
  deriving Show

initStation :: Station
initStation = Station

stepStation :: [ForwMsg] -> State Station StationFeedback
stepStation = return . (,True,Nothing) . recvMsgs

recvMsgs :: [ForwMsg] -> MsgResult
recvMsgs []  = Empty
recvMsgs [ForwMsg uid] = Success uid
recvMsgs ls   = Conflict $ map _uid ls
