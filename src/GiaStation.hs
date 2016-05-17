{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GiaStation
    ( stepStation
    , Station
    , initStation
    ) where


import Control.Exception.Base( assert )
import Control.Lens
import Control.Monad.State.Strict
import Data.List( foldl' )

import Interface( ForwSignal(..)
                , ForwMsg(..)
                , UserID
                , StationFeedback
                , MsgResult(..)
                )
import TreeCSMACD
import Prelude hiding( subtract )
import Signals( subtract, add, getNoise )


type StationTree = Maybe (Tree ForwSignal)

-- 1. useWindow: will a window be skipped or not
-- 2. mReconstructed: while on left leaf, maybe reconstructed right leaf
-- 3. msgResult: result of current window (success, empty or conflict)
-- 4. label: a signal stored for given node
type StepResult = (Bool, Maybe UserID, MsgResult, ForwSignal)

data Station = Station {
        _tree :: StationTree,
        _perror :: Double
    } deriving Show

makeLenses ''Station

initStation :: Double -> Station
initStation qs = Station initTree qs

stepStation :: ForwSignal -> State Station StationFeedback
stepStation input = do
        mt <- use tree
        qs <- use perror
        let (useWindow, mReconstructed, result, signal) = stepStationInternal qs mt input
        tree %= updateTree result signal
        return (result, useWindow, mReconstructed)

-- TODO: cancel noise for SICTA and RSICTA in successful left leafs.
stepStationInternal :: Double -> StationTree -> ForwSignal -> StepResult
stepStationInternal qs mt input =
    case leftUndef =<< mt of
        Nothing -> (True, Nothing, recvMsgs qs input, input)
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
                    -- TODO: Check for user's ability to handle two acks
                    -- (once while in left branch with sum noises, second
                    -- in right branch with parent noise only)
                    mReconstructed = tryReconstructSignal qs parentInput input
                    -- mReconstructed = Nothing
                in (True, mReconstructed, recvMsgs qs input, input)
            -- Right == user decided to transmit later
            else (stepRightNode qs input mt)
        Just _ -> error "leftUndef is not undef, impossible"

stepRightNode :: Double -> ForwSignal -> StationTree -> StepResult
stepRightNode = stepRightNodeSicta

-- SICTA
stepRightNodeSicta :: Double -> ForwSignal -> StationTree -> StepResult
stepRightNodeSicta qs inp _ = (False, Nothing, recvMsgs qs inp, inp)

-- RSICTA
stepRightNodeRSicta :: Double -> ForwSignal -> StationTree -> StepResult
stepRightNodeRSicta qs inp mt = (useWindow, Nothing, recvMsgs qs inp, inp)
    where isEmptyBrother (Just (ELeaf _ _)) = True
          isEmptyBrother _ = False

          brother = bro mt
          useWindow = isConflictInput qs inp && not (isEmptyBrother brother)

-- My modification
stepRightNodeNoiseElimination :: Double -> ForwSignal -> StationTree -> StepResult
stepRightNodeNoiseElimination qs _inp mt =
        (False, mReconstruct, recvMsgs qs restored, restored)
    where left = eliminateNoiseOnBrother qs brother
          Just (Undef _curLabel parent) = leftUndef =<< mt
          brother = bro mt
          restored = assert (getNoise left == 0.0) $ parent `subtract` left
          mReconstruct = maybeReconstruct qs restored

-- XXX: works only if currentNode is RIGHT
bro :: StationTree -> StationTree
bro mt = brotherNode lbl mt
    where Just (Undef lbl _minp) = leftUndef =<< mt

recvMsgs :: Double -> ForwSignal -> MsgResult
recvMsgs _ (ForwSignal [] _) = Empty
recvMsgs qs (ForwSignal [ForwMsg uid] n) | n <= (1.0 - qs) = Success uid
recvMsgs _ (ForwSignal ls _) = Conflict $ map _uid ls

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

tryReconstructSignal :: Double -> ForwSignal -> ForwSignal -> Maybe UserID
tryReconstructSignal qs parent left = maybeReconstruct qs $ parent `subtract` left

maybeReconstruct :: Double -> ForwSignal -> Maybe UserID
maybeReconstruct qs s = case recvMsgs qs s of
                            Success uid -> Just uid
                            _ -> Nothing

-- FOR RSICTA
isConflictInput :: Double -> ForwSignal -> Bool
isConflictInput qs s = case recvMsgs qs s of
                           Conflict _ -> True
                           _ -> False

-- backward modulation on receiver
recoverSignal :: Double -> ForwSignal -> ForwSignal
recoverSignal qs s = case recvMsgs qs s of
                         Success uid -> ForwSignal [ForwMsg uid] 0.0
                         _ -> s

-- accepts a fully completed subtree (high one!) and returns
-- its parent input free from noise
eliminateNoiseOnBrother :: Double -> StationTree -> ForwSignal
eliminateNoiseOnBrother qs t = foldl' add emptySignal noiselessSuccesses
        where noiselessSuccesses = map (recoverSignal qs) successes
              successes = successLeafs t
              emptySignal = ForwSignal [] 0.0
