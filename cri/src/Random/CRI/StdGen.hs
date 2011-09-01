{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.StdGen where

import Control.Monad.Primitive
import Control.Monad.State
import Data.StateRef
import Random.CRI.Generates
import Random.CRI.Utils
import qualified System.Random as R

instance (R.Random a, Monad m) => Generates (StateT R.StdGen m) a where
  random    = stateWrap  R.random
  -- randomR r = stateWrap (R.randomR r)

newtype Std m = Std { unStd :: Ref m R.StdGen }

instance (PrimMonad m, R.Random a) => PrimGenerates m Std a where
  randomPrim    p = primWrap  R.random     (unStd p)
  -- randomPrimR r p = primWrap (R.randomR r) (unStd p)

mkStd :: (HasRef m, PrimMonad m) => Int -> m (Std m)
mkStd n = do
              r <- newRef (R.mkStdGen n)
              return (Std r)
