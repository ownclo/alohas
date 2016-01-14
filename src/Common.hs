{-# LANGUAGE RankNTypes #-}

module Common where

import Control.Lens
import Control.Monad.State

import Data.List( genericLength )
import Interface( MsgResult(..) )

roll :: MonadState s m => Lens' s [a] -> m a
roll alens = do
    x:xs <- use alens
    alens .= xs
    return x

append :: MonadState s m => Lens' s [a] -> a -> m ()
append alens val = alens <>= [val]

prepend :: MonadState s m => Lens' s [a] -> a -> m ()
prepend alens val = alens %= (val:)

mean :: (Real a, Fractional b) => [a] -> b
mean l = realToFrac (sum l) / genericLength l

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing  _   = return ()
whenMaybe (Just x) a = a x

isConflict :: MsgResult -> Bool
isConflict (Conflict _) = True
isConflict _ = False

isEmpty :: MsgResult -> Bool
isEmpty Empty = True
isEmpty _ = False

isSuccess :: MsgResult -> Bool
isSuccess (Success _) = True
isSuccess _ = False
