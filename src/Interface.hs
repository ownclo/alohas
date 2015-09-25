module Interface where

data UserID = UID Int
  deriving (Show, Eq)

data MsgQueueLen = INF | Bounded Int
                 deriving Show

data ForwMsg = ForwMsg UserID
  deriving Show

data MsgResult = Empty
               | Success
               | Conflict
               deriving (Eq, Show)

data MsgSourceType = BoundedQueue
                   | TwoBufferQueue
