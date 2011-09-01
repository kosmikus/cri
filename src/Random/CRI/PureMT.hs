{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.PureMT where

import Control.Monad.Primitive
import Control.Monad.State
import Data.StateRef
import qualified System.Random.Mersenne.Pure64 as R
import Random.CRI.Generates
import Random.CRI.Utils

instance (Monad m) => Generates (StateT R.PureMT m) Int where
  random = stateWrap R.randomInt

instance (Monad m) => Generates (StateT R.PureMT m) Double where
  random = stateWrap R.randomDouble

data PMT m = PMT { unPMT :: Ref m R.PureMT }

instance (PrimMonad m) => PrimGenerates m PMT Int where
  randomPrim (PMT p) = primWrap R.randomInt p

instance (PrimMonad m) => PrimGenerates m PMT Double where
  randomPrim (PMT p) = primWrap R.randomDouble p
