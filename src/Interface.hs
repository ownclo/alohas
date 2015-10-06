module Interface where

data UserID = UID Int
  deriving (Show, Eq)

data MsgQueueLen = INF | Bounded Int
                 deriving Show

data ForwMsg = ForwMsg { _uid :: UserID }
  deriving Show

data MsgResult = Empty
               | Success UserID
               | Conflict [UserID]
               deriving (Eq, Show)

data MsgSourceType = BoundedQueue
                   | TwoBufferQueue
