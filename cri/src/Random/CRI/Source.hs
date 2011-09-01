{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleContexts #-}
module Random.CRI.Source where

data ImpliedGen (m :: * -> *) = ImpliedGen

random  :: Source       m ImpliedGen a => m a
random    = grandom    ImpliedGen

randomR :: RangedSource m ImpliedGen a => (a, a) -> m a
randomR r = grandomR r ImpliedGen

class Monad m => Source m g a where
  grandom  :: g m -> m a

class Monad m => RangedSource m g a where
  grandomR :: (a, a) -> g m -> m a

