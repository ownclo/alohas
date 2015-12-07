module BoundedQueue where

import Control.Monad.State.Strict
import Data.Maybe( listToMaybe )

import Interface( MsgQueueLen(..), MsgSourceType(..) )

type MsgSource a = BQ a
type MsgParams = MsgQueueLen
type BQ a = [a]

sourceType :: MsgSourceType
sourceType = BoundedQueue

init :: BQ a
init = []

canGenerate :: MsgParams -> BQ a -> Bool
canGenerate mqlen = hasEmptySpace mqlen . length

hasEmptySpace :: MsgParams -> Int -> Bool
hasEmptySpace INF _ = True
hasEmptySpace (Bounded bound) qsize = qsize < bound

storeGenerate :: a -> State (BQ a) ()
storeGenerate a = modify (++ [a])

getTransmit :: BQ a -> Maybe a
getTransmit = listToMaybe

shiftTransmit' :: BQ a -> BQ a
shiftTransmit' = tail

shiftTransmit :: State (BQ a) ()
shiftTransmit = modify shiftTransmit'

tickDelays :: (a -> a) -> State (BQ a) ()
tickDelays tick = modify $ fmap tick

dropMsg :: State (BQ a) ()
dropMsg = return ()
