{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GiaStation
    ( stepStation
    , Station
    , initStation
    ) where


import Control.Lens
import Control.Monad.State.Strict
import Data.List( (\\), foldl1' )

import Interface( ForwSignal(..)
                , ForwMsg(..)
                , UserID
                , StationFeedback
                , MsgResult(..)
                )
import TreeCSMACD
import Signals


type StationTree = Maybe (Tree ForwSignal)

data Station = Station {
        _tree :: StationTree
    } deriving Show

makeLenses ''Station

initStation :: Station
initStation = Station initTree

stepStation :: ForwSignal -> State Station StationFeedback
stepStation input = do
        let msgResult = recvMsgs input
        mt <- use tree
        tree %= updateTree msgResult input
        let (useWindow, mReconstructed) = stepStationInternal mt input
        return (msgResult, useWindow, mReconstructed)

stepStationInternal :: StationTree -> ForwSignal -> (Bool, Maybe UserID)
stepStationInternal mt input =
    case leftUndef =<< mt of
        Nothing -> (True, Nothing)
        Just (Undef lbl _minp) ->
            let isLeft = head lbl  -- == True
                brother = brotherNode lbl mt in
            -- Left == user decided to retransmit immediately.
            if isLeft then
                -- It can be proven that when current leaf is left
                -- one, the brother node is a) present and b) it is
                -- Undef. Further, by construction, the payload of
                -- the Undef node is the same as its parent node, so
                -- for interference cancellation one can use it's
                -- payload and input.
                let Just (Undef _broLabel parentInput) = brother
                    mReconstructed = tryReconstructSignal parentInput input
                in (True, mReconstructed)
            -- Right == user decided to transmit later
            else (useWindowOnRightNode input brother, Nothing)
        Just _ -> error "leftUndef is not undef, impossible"

-- SICTA
useWindowOnRightNode :: ForwSignal -> StationTree -> Bool
useWindowOnRightNode _ _ = False

-- RSICTA
useWindowOnRightNodeRSicta :: ForwSignal -> StationTree -> Bool
useWindowOnRightNodeRSicta inp brother = isConflictInput inp
                                      && not (isEmptyBrother brother)
    where isEmptyBrother (Just (ELeaf _ _)) = True
          isEmptyBrother _ = False

ns :: Double
ns = 1.0 - qs where qs = 0.3

recvMsgs :: ForwSignal -> MsgResult
recvMsgs (ForwSignal [] _) = Empty
recvMsgs (ForwSignal [ForwMsg uid] n) | n <= ns = Success uid
recvMsgs (ForwSignal ls _) = Conflict $ map _uid ls

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

tryReconstructSignal :: ForwSignal -> ForwSignal -> Maybe UserID
tryReconstructSignal parent left = maybeReconstruct $ subtractSignals parent left

maybeReconstruct :: ForwSignal -> Maybe UserID
maybeReconstruct s = case recvMsgs s of
                         Success uid -> Just uid
                         _ -> Nothing

isConflictInput :: ForwSignal -> Bool
isConflictInput s = case recvMsgs s of
                        Conflict _ -> True
                        _ -> False

-- backward modulation on receiver
recoverSignal :: ForwSignal -> ForwSignal
recoverSignal s = case recvMsgs s of
                      Success uid -> ForwSignal [ForwMsg uid] 0.0
                      _ -> s

-- accepts a fully completed subtree (high one!) and returns
-- its parent input free from noise
eliminateNoiseOnBrother :: StationTree -> ForwSignal
eliminateNoiseOnBrother bro = foldl1' addSignals noiselessSuccesses
        where noiselessSuccesses = map recoverSignal successes
              successes = successLeafs bro
