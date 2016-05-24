{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GiaStation
    ( stepStation
    , Station
    , initStation
    , tree
    ) where


import Control.Exception.Base( assert )
import Control.Lens
import Control.Monad.State.Strict
import Data.List( foldl' )

import Interface( ForwSignal(..)
                , ForwMsg(..)
                , UserID
                , StationFeedback
                , ModelFeedback
                , MsgResult(..)
                )
import TreeCSMACD
import Prelude hiding( subtract )
import Signals
import Common( roll )


type StationTree = Maybe (Tree ForwSignal)

-- 1. useWindow: will a window be skipped or not
-- 2. mReconstructed: while on left leaf, maybe reconstructed right leaf
-- 3. msgResult: result of current window (success, empty or conflict)
-- 4. label: a signal stored for given node
type StepResult = (Bool, Maybe UserID, MsgResult, ForwSignal)

data Station = Station {
        _tree :: StationTree,
        _baseSNR :: Double,
        _randStream :: [Double]
    } deriving Show

makeLenses ''Station

initStation :: Double -> [Double] -> Station
initStation baseSnr stream = Station initTree baseSnr stream

stepStation :: ForwSignal -> State Station (ModelFeedback, StationFeedback)
stepStation input = do
        (useWindow, mReconstructed, result, signal) <- stepStationInternal input
        tree %= updateTree result signal
        isOver <- uses tree isResolved
        let feedBack = (result, useWindow, mReconstructed)
        return ((isOver, useWindow), feedBack)

genIsError :: ForwSignal -> State Station Bool
genIsError sig = do
    rval <- roll randStream
    baseSnr <- use baseSNR
    let l = 424 -- # bits, God only knows why
        pError = probErrorSignal sig baseSnr l
    return $ rval < pError

stepStationInternal :: ForwSignal -> State Station StepResult
stepStationInternal input = do
    mt <- use tree
    isError <- genIsError input
    case leftUndef =<< mt of
        Nothing -> return (True, Nothing, recvMsgs isError input, input)
        Just (Undef lbl _minp) ->
            let isLeft = head lbl  -- == True
                brother = brotherNode lbl mt in
            if isLeft then do
                -- It can be proven that when current leaf is left
                -- one, the brother node is a) present and b) it is
                -- Undef. Further, by construction, the payload of
                -- the Undef node is the same as its parent node, so
                -- for interference cancellation one can use it's
                -- payload and input.
                let Just (Undef _broLabel parentInput) = brother
                    mReconstructed = Nothing
                -- mReconstructed <- tryReconstructSignal parentInput input
                return (True, mReconstructed, recvMsgs isError input, input)
            -- Right == user decided to transmit later
            else return (stepRightNode isError input mt)
        Just _ -> error "leftUndef is not undef, impossible"

stepRightNode :: Bool -> ForwSignal -> StationTree -> StepResult
stepRightNode = stepRightNodeNoiseElimination

-- SICTA
stepRightNodeSicta :: Bool -> ForwSignal -> StationTree -> StepResult
stepRightNodeSicta qs inp _ = (False, Nothing, recvMsgs qs inp, inp)

-- RSICTA
stepRightNodeRSicta :: Bool -> ForwSignal -> StationTree -> StepResult
stepRightNodeRSicta qs inp mt = (useWindow, Nothing, recvMsgs qs inp, inp)
    where isEmptyBrother (Just (ELeaf _ _)) = True
          isEmptyBrother _ = False

          brother = bro mt
          useWindow = isConflictInput qs inp && not (isEmptyBrother brother)

-- My modification
stepRightNodeNoiseElimination :: Bool -> ForwSignal -> StationTree -> StepResult
stepRightNodeNoiseElimination qs _inp mt =
        (False, mReconstruct, recvMsgs qs restored, restored)
    where left = eliminateNoiseOnBrother brother
          Just (Undef _curLabel parent) = leftUndef =<< mt
          brother = bro mt
          restored = assert (getNoise left == 0.0) $ parent `subtract` left
          mReconstruct = maybeReconstruct qs restored

-- XXX: works only if currentNode is RIGHT
bro :: StationTree -> StationTree
bro mt = brotherNode lbl mt
    where Just (Undef lbl _minp) = leftUndef =<< mt

recvMsgs :: Bool -> ForwSignal -> MsgResult
recvMsgs _ (ForwSignal [] _) = Empty
recvMsgs False (ForwSignal [ForwMsg uid] _n) = Success uid
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

tryReconstructSignal :: ForwSignal -> ForwSignal -> State Station (Maybe UserID)
tryReconstructSignal parent left = do
        let right = parent `subtract` left
        isError <- genIsError right
        return $ maybeReconstruct isError right

maybeReconstruct :: Bool -> ForwSignal -> Maybe UserID
maybeReconstruct isError s = case recvMsgs isError s of
                                 Success uid -> Just uid
                                 _ -> Nothing

-- FOR RSICTA
isConflictInput :: Bool -> ForwSignal -> Bool
isConflictInput isError s = case recvMsgs isError s of
                                Conflict _ -> True
                                _ -> False

-- backward modulation on receiver
recoverSignal :: ForwSignal -> ForwSignal
recoverSignal s = case recvMsgs False s of
                      Success uid -> ForwSignal [ForwMsg uid] 0.0
                      _ -> s

-- accepts a fully completed subtree (high one!) and returns
-- its parent input free from noise
eliminateNoiseOnBrother :: StationTree -> ForwSignal
eliminateNoiseOnBrother t = foldl' add emptySignal noiselessSuccesses
        where noiselessSuccesses = map recoverSignal successes
              successes = successLeafs t
              emptySignal = ForwSignal [] 0.0
