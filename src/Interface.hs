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

-- second element is a list of recovered transmissions
type StationFeedback = (MsgResult, [[UserID]])

data MsgSourceType = BoundedQueue
                   | TwoBufferQueue
