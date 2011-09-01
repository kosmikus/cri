module Random.CRI.Utils where

import Control.Monad.Reader
import Control.Monad.State
import Data.StateRef

{-# INLINE readerWrap #-}
readerWrap :: MonadReader g m => (g -> m a) -> m a
readerWrap f = ask >>= f

{-# INLINE stateWrap #-}
stateWrap :: MonadState g m => (g -> (a, g)) -> m a
stateWrap f = do
  g <- get
  let (x, g') = f g
  put g'
  return x

{-# INLINE primWrap #-}
primWrap :: Monad m => (g -> (a, g)) -> Ref m g -> m a
primWrap f p = do
  g <- readRef p
  let (x, g') = f g
  writeRef p g'
  return x
