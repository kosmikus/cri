{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.LFG where

import Control.Monad.Primitive
import Control.Monad.State
import Data.StateRef
import Random.CRI.Generates
import Random.CRI.Utils
import qualified Random.LFG as R

instance Monad m => Generates (StateT R.Gen m) Double where
  random = stateWrap R.step

newtype LFG m = LFG { unLFG :: Ref m R.Gen }

instance (PrimMonad m) => PrimGenerates m LFG Double where
  randomPrim (LFG p) = primWrap R.step p
