module Interface where

data UserID = UID Int
  deriving (Show, Eq)

data MsgQueueLen = INF | Bounded Int
                 deriving Show

data ForwMsg = ForwMsg { _uid :: UserID }
  deriving (Show, Eq)

data MsgResult = Empty
               | Success UserID
               | Conflict [UserID]
               deriving (Eq, Show)

-- second element is an indicator as to whether the window should be
-- counted in statistics.
type StationFeedback = (MsgResult, Bool)

data MsgSourceType = BoundedQueue
                   | TwoBufferQueue
