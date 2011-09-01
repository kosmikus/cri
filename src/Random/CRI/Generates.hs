{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, FlexibleContexts #-}
module Random.CRI.Generates where

import Control.Monad.Primitive

class Monad m => Generates m a where
  random  :: m a
  -- randomR :: (a, a) -> m a

class PrimMonad m => PrimGenerates m g a where
  randomPrim  :: g m -> m a
  -- randomPrimR :: (a, a) -> g m -> m a
