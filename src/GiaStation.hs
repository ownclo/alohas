{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GiaStation
    ( stepStation
    , Station
    , initStation
    , tree

    , falseConflicts
    ) where


import Control.Applicative
import Control.Exception.Base( assert )
import Control.Lens
import Control.Monad.State.Strict
import Data.List( foldl' )
import Data.Maybe( fromJust )

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
import Common( roll, isSuccess )


type IsError = Bool
type StoredSignal = (ForwSignal, Maybe IsError)
type StationTree = Maybe (Tree StoredSignal)

-- 1. useWindow: will a window be skipped or not
-- 2. mReconstructed: while on left leaf, maybe reconstructed right leaf
-- 3. msgResult: result of current window (success, empty or conflict)
-- 4. label: a signal stored for given node
type StepResult = (Bool, Maybe UserID, MsgResult, StoredSignal)

data Station = Station {
        _tree :: StationTree,
        _baseSNR :: Double,
        _subtractSNR :: Double,
        _randStream :: [Double],

        _falseConflicts :: Int
    } deriving Show

makeLenses ''Station

initStation :: Double -> Double -> [Double] -> Station
initStation baseSnr subSnr stream = Station initTree baseSnr subSnr stream 0

stepStation :: ForwSignal -> State Station (ModelFeedback, StationFeedback)
stepStation input = do
        (useWindow, mReconstructed, result, signal) <- stepStationInternal (clearEmptySignal input)
        !_falseClts <- use falseConflicts
        when (falseConflict result signal) $ falseConflicts += 1
        tree %= updateTree result signal
        isOver <- uses tree isResolved
        let feedBack = (result, useWindow, mReconstructed)
        return ((isOver, useWindow), feedBack)

falseConflict :: MsgResult -> StoredSignal -> Bool
falseConflict (Conflict _) ((ForwSignal [_] _), _) = True
falseConflict _ _ = False

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
        Nothing -> return (True, Nothing, recvMsgs isError input, (input, Nothing))
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
                mReconstructed <- stepLeftNode input brother
                return (True, mReconstructed, recvMsgs isError input, (input, Nothing))
            -- Right == user decided to transmit later
            else stepRightNode isError input mt
        Just _ -> error "leftUndef is not undef, impossible"

stepLeftNode :: ForwSignal -> StationTree -> State Station (Maybe UserID)
-- stepLeftNode input (Just (Undef broLabel (parentInput, _mParentNoise))) = do
--     subSnr <- use subtractSNR
--     let broSignal = subtract parentInput input subSnr
--     isError <- if isEmptyInput input then return True else genIsError broSignal
--     tree %= modifyNodeFromLabel broLabel (parentInput, Just isError)
--     return $ maybeReconstruct isError broSignal
-- stepLeftNode _ other = error $ show other
stepLeftNode _ _ = return Nothing

stepRightNode :: Bool -> ForwSignal -> StationTree -> State Station StepResult
stepRightNode = stepRightNodeNoiseElimination

-- SICTA
stepRightNodeSicta :: Bool -> ForwSignal -> StationTree -> State Station StepResult
stepRightNodeSicta qs inp _ = return (False, Nothing, recvMsgs qs inp, (inp, Nothing))

-- RSICTA
stepRightNodeRSicta :: Bool -> ForwSignal -> StationTree -> State Station StepResult
stepRightNodeRSicta qs inp mt = return (useWindow, Nothing, recvMsgs qs inp, (inp, Nothing))
    where isEmptyBrother (Just (ELeaf _ _)) = True
          isEmptyBrother _ = False

          brother = bro mt
          useWindow = isConflictInput qs inp && not (isEmptyBrother brother)

-- My modification
stepRightNodeNoiseElimination :: Bool -> ForwSignal -> StationTree -> State Station StepResult
stepRightNodeNoiseElimination _qs _inp mt = do
        subSnr <- use subtractSNR
        let left = eliminateNoiseOnBrother subSnr brother
            leftDirty = fst . getPayload . fromJust $ brother

            Just (Undef _curLabel (parent, curIsError)) = leftUndef =<< mt
            brother = bro mt
            restored = subtract parent left subSnr
            restoredDirty = subtract parent leftDirty subSnr

        newIsError <- if isEmptyInput left then return True else genIsError restored

        -- XXX: if curIsError is present, we must use it.
        let isError = case curIsError of
                          Just True -> newIsError
                          Just False -> False
                          Nothing -> newIsError

            mReconstruct = maybeReconstruct isError restored
            resLeft = recvMsgs newIsError restored
            resLeftDirty = recvMsgs (fromJust curIsError) restoredDirty

            isDirtyOk = isSuccess resLeftDirty
            isCleanOk = isSuccess resLeft

            -- toStore = if isCleanOk then restored else restoredDirty
            toStore = restored
            -- result = if isCleanOk then resLeft else resLeftDirty
            result = resLeft

        return (False, mReconstruct, result, (toStore, Nothing))

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

maybeReconstruct :: Bool -> ForwSignal -> Maybe UserID
maybeReconstruct isError s = case recvMsgs isError s of
                                 Success uid -> Just uid
                                 _ -> Nothing

-- FOR RSICTA
isConflictInput :: Bool -> ForwSignal -> Bool
isConflictInput isError s = case recvMsgs isError s of
                                Conflict _ -> True
                                _ -> False

clearEmptySignal :: ForwSignal -> ForwSignal
clearEmptySignal (ForwSignal [] _) = ForwSignal [] 0.0
clearEmptySignal s = s

isEmptyInput :: ForwSignal -> Bool
isEmptyInput (ForwSignal [] _) = True
isEmptyInput _ = False

-- backward modulation on receiver
recoverSignal :: ForwSignal -> ForwSignal
recoverSignal s = case recvMsgs False s of
                      Success uid -> ForwSignal [ForwMsg uid] 0.0
                      _ -> s

-- accepts a fully completed subtree (high one!) and returns
-- its parent input free from noise
eliminateNoiseOnBrother :: Double -> StationTree -> ForwSignal
eliminateNoiseOnBrother subSnr t = foldl' (add subSnr) emptySignal noiselessSuccesses
        where noiselessSuccesses = map (recoverSignal . fst) successes
              successes = successLeafs t
              emptySignal = ForwSignal [] 0.0
