{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GiaStation
    ( stepStation
    , Station
    , initStation
    ) where


import Control.Applicative( (<$>) )
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
        (useWindow, mReconstructed) <- case cur of
            Nothing -> return (True, Nothing)
            Just (Undef lbl _minp) -> do
                let isLeft = head lbl  -- == True
                -- Left == user decided to retransmit immediately.
                if isLeft then do
                      -- It can be proven that when current leaf is left
                      -- one, the brother node is a) present and b) it is
                      -- Undef. Further, by construction, the payload of
                      -- the Undef node is the same as its parent node, so
                      -- for interference cancellation one can use it's
                      -- payload and input.
                      brother <- brotherNode lbl <$> use tree
                      let Just (Undef _broLabel parentInput) = brother
                          mReconstructed = tryReconstructSignal parentInput inp
                      return (True, mReconstructed)
                  else return (False, Nothing)
            Just _ -> error "leftUndef is not undef, impossible"
        return (msgResult, useWindow, mReconstructed)

recvMsgs :: StationInput -> MsgResult
recvMsgs []  = Empty
recvMsgs [ForwMsg uid] = Success uid
recvMsgs ls   = Conflict $ map _uid ls

inputSignals :: StationInput -> InputSignals
inputSignals = map _uid

-- get the brother node in the tree
-- e.g. tree: a - b
--              \ c
-- brotherNode b tree -> c
-- brotherNode c tree -> b
-- brotherNode a tree -> none
brotherNode :: Label -> StationTree -> StationTree
brotherNode [] _ = Nothing  -- root has no brother node
brotherNode _lbl Nothing = Nothing
brotherNode (lastLabel:rst) (Just t) = getNodeFromLabel (not lastLabel:rst) t

tryReconstructSignal :: InputSignals -> InputSignals -> Maybe UserID
tryReconstructSignal parent left = maybeReconstruct $ parent \\ left

maybeReconstruct :: InputSignals -> Maybe UserID
maybeReconstruct [s] = Just s
maybeReconstruct _ = Nothing
