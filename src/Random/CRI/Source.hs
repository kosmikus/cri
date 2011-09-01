{-# LANGUAGE MultiParamTypeClasses #-}
module Random.CRI.Source where

import Control.Monad.Primitive

class Monad m => Source m a where
  random   :: m a

class Monad m => RangedSource m a where
  randomR  :: (a, a) -> m a

class PrimMonad m => PrimSource m g a where
  prandom  :: g m -> m a

class PrimMonad m => PrimRangedSource m g a where
  prandomR :: (a, a) -> g m -> m a
