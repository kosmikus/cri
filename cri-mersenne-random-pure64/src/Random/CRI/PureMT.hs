{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.PureMT where

import Control.Monad.Primitive
import Control.Monad.State
import Data.Int
import Data.StateRef
import Data.Word
import qualified System.Random.Mersenne.Pure64 as R
import Random.CRI
import Random.CRI.Utils

instance (Monad m) => Source (StateT R.PureMT m) ImpliedGen Double where
  grandom _ = stateWrap R.randomDouble

instance (Monad m) => Source (StateT R.PureMT m) ImpliedGen Int    where
  grandom _ = stateWrap R.randomInt

instance (Monad m) => Source (StateT R.PureMT m) ImpliedGen Int64  where
  grandom _ = stateWrap R.randomInt64

instance (Monad m) => Source (StateT R.PureMT m) ImpliedGen Word   where
  grandom _ = stateWrap R.randomWord

instance (Monad m) => Source (StateT R.PureMT m) ImpliedGen Word64 where
  grandom _ = stateWrap R.randomWord64

-- TODO: add ranged instances

newtype PureMT m = PureMT { unPureMT :: Ref m R.PureMT }

instance (PrimMonad m) => Source m PureMT Double where
  grandom (PureMT p) = primWrap R.randomDouble p

instance (PrimMonad m) => Source m PureMT Int    where
  grandom (PureMT p) = primWrap R.randomInt    p

instance (PrimMonad m) => Source m PureMT Int64  where
  grandom (PureMT p) = primWrap R.randomInt64  p

instance (PrimMonad m) => Source m PureMT Word   where
  grandom (PureMT p) = primWrap R.randomWord   p

instance (PrimMonad m) => Source m PureMT Word64 where
  grandom (PureMT p) = primWrap R.randomWord64 p

pureMT :: (HasRef m, Monad m) => Word64 -> m (PureMT m)
pureMT x = liftM PureMT (newRef (R.pureMT x))
