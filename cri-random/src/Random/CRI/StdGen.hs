{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.StdGen where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State
import Data.StateRef
import Random.CRI
import Random.CRI.Utils
import qualified System.Random as R

instance (R.Random a, Monad m) => Source       (StateT R.StdGen m) ImpliedGen a where
  grandom    _ = stateWrap  R.random

instance (R.Random a, Monad m) => RangedSource (StateT R.StdGen m) ImpliedGen a where
  grandomR r _ = stateWrap (R.randomR r)

newtype StdGen m = StdGen { unStdGen :: Ref m R.StdGen }

instance (PrimMonad m, R.Random a) => Source       m StdGen a where
  grandom    (StdGen p) = primWrap  R.random     p

instance (PrimMonad m, R.Random a) => RangedSource m StdGen a where
  grandomR r (StdGen p) = primWrap (R.randomR r) p

mkStdGen :: (HasRef m, PrimMonad m) => Int -> m (StdGen m)
mkStdGen n = liftM StdGen (newRef (R.mkStdGen n))

newStdGen :: IO (StdGen IO)
newStdGen = liftM StdGen (R.newStdGen >>= newRef)
