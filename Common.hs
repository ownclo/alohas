{-# LANGUAGE RankNTypes #-}

module Common where

import Control.Lens
import Control.Monad.State

roll :: MonadState s m => Lens' s [a] -> m a
roll alens = do
    x:xs <- use alens
    alens .= xs
    return x

append :: MonadState s m => Lens' s [a] -> a -> m ()
append alens val = alens <>= [val]
