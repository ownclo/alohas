module Interface where

data UserID = UID Int
  deriving (Show, Eq)

data MsgQueueLen = INF | Bounded Int
                 deriving Show

data ForwMsg = ForwMsg { _uid :: UserID }
  deriving (Show, Eq)

type Noise = Double

data ForwSignal = ForwSignal {
        _userInput :: [ForwMsg],
        _noise :: Noise
    } deriving (Eq, Show)

data MsgResult = Empty
               | Success UserID
               | Conflict [UserID]
               deriving (Eq, Show)

-- Second element is an indicator as to whether the window should be
-- counted in statistics.
-- ThirdElement is an indicator if another message was preliminarily
-- reconstructed.
type StationFeedback = (MsgResult, Bool, Maybe UserID)

data MsgSourceType = BoundedQueue
                   | TwoBufferQueue
