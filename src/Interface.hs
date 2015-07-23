module Interface where

data UserID = UID Int
  deriving (Show, Eq)

data ForwMsg = ForwMsg UserID
  deriving Show

data MsgResult = Empty
               | Success
               | Conflict
               deriving (Eq, Show)
