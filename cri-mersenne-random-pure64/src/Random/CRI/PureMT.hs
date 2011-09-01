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

instance (Monad m) => Source (StateT R.PureMT m) Double where
  random = stateWrap R.randomDouble

instance (Monad m) => Source (StateT R.PureMT m) Int    where
  random = stateWrap R.randomInt

instance (Monad m) => Source (StateT R.PureMT m) Int64  where
  random = stateWrap R.randomInt64

instance (Monad m) => Source (StateT R.PureMT m) Word   where
  random = stateWrap R.randomWord

instance (Monad m) => Source (StateT R.PureMT m) Word64 where
  random = stateWrap R.randomWord64

-- TODO: add ranged instances

newtype PureMT m = PureMT { unPureMT :: Ref m R.PureMT }

instance (PrimMonad m) => PrimSource m PureMT Double where
  prandom (PureMT p) = primWrap R.randomDouble p

instance (PrimMonad m) => PrimSource m PureMT Int    where
  prandom (PureMT p) = primWrap R.randomInt    p

instance (PrimMonad m) => PrimSource m PureMT Int64  where
  prandom (PureMT p) = primWrap R.randomInt64  p

instance (PrimMonad m) => PrimSource m PureMT Word   where
  prandom (PureMT p) = primWrap R.randomWord   p

instance (PrimMonad m) => PrimSource m PureMT Word64 where
  prandom (PureMT p) = primWrap R.randomWord64 p

pureMT :: (HasRef m, Monad m) => Word64 -> m (PureMT m)
pureMT x = liftM PureMT (newRef (R.pureMT x))
