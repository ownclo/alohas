{-# LANGUAGE RankNTypes #-}

module Common where

import Control.Lens
import Control.Monad.State

import Data.List( genericLength )

roll :: MonadState s m => Lens' s [a] -> m a
roll alens = do
    x:xs <- use alens
    alens .= xs
    return x

append :: MonadState s m => Lens' s [a] -> a -> m ()
append alens val = alens <>= [val]

mean :: (Real a, Fractional b) => [a] -> b
mean l = realToFrac (sum l) / genericLength l

whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing  _   = return ()
whenMaybe (Just x) a = a x
