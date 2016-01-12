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
                , UserID
                , StationFeedback
                , MsgResult(..)
                )
import TreeCSMACD

type StationInput = [ForwMsg]
type StationHistory = [StationInput]

type InputSignals = [UserID]
type StationTree = Maybe (Tree InputSignals)

data Station = Station {
        _tree :: StationTree
    } deriving Show

makeLenses ''Station

initStation :: Station
initStation = Station initTree

stepStation :: StationInput -> State Station StationFeedback
stepStation input = do
        let msgResult = recvMsgs input
            inp = inputSignals input
        mt <- use tree
        let cur = leftUndef =<< mt
        tree %= updateTree msgResult inp
        useWindow <- case cur of
            Nothing -> return True
            Just (Undef lbl minp) -> do
                let isLeft = head lbl == True
                if isLeft  -- for those who decided to retransmit immediately
                  then do
                      -- tree %= cancelInterference lbl inp
                      return True
                  else do
                      -- when (inp /= minp) $
                      --     error $ "For RIGHT window the inp = " ++ show inp ++
                      --             " != pre-set = " ++ show minp
                      return False
            Just _ -> error "leftUndef is not undef, impossible"
        return (msgResult, useWindow)

recvMsgs :: StationInput -> MsgResult
recvMsgs []  = Empty
recvMsgs [ForwMsg uid] = Success uid
recvMsgs ls   = Conflict $ map _uid ls

inputSignals :: StationInput -> InputSignals
inputSignals = map _uid

cancelInterference :: Label -> InputSignals -> StationTree -> StationTree
cancelInterference lbl inp mt = undefined

subtractSignals :: StationInput -> StationInput -> StationInput
subtractSignals = flip (\\)
