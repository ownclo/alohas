module Interface where

data ForwMsg = ForwMsg
  deriving Show

data MsgResult = Empty
               | Success
               | Conflict
               deriving (Eq, Show)
