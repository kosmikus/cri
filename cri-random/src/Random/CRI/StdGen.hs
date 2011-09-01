{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.StdGen where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State
import Data.StateRef
import Random.CRI
import Random.CRI.Utils
import qualified System.Random as R

instance (R.Random a, Monad m) => Source       (StateT R.StdGen m) a where
  random    = stateWrap  R.random

instance (R.Random a, Monad m) => RangedSource (StateT R.StdGen m) a where
  randomR r = stateWrap (R.randomR r)

newtype StdGen m = StdGen { unStdGen :: Ref m R.StdGen }

instance (PrimMonad m, R.Random a) => PrimSource       m StdGen a where
  prandom    (StdGen p) = primWrap  R.random     p

instance (PrimMonad m, R.Random a) => PrimRangedSource m StdGen a where
  prandomR r (StdGen p) = primWrap (R.randomR r) p

mkStdGen :: (HasRef m, PrimMonad m) => Int -> m (StdGen m)
mkStdGen n = liftM StdGen (newRef (R.mkStdGen n))
