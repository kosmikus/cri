{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Random.CRI.MWC where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Reader
import Random.CRI.Generates
import Random.CRI.Utils
import System.Random.MWC

newtype MWC m = MWC { unMWC :: Gen (PrimState m) }

instance (PrimMonad m, Variate a) => PrimGenerates m MWC a where
  randomPrim    g = uniform    (unMWC g)
  -- randomPrimR r g = uniformR r (unMWC g)

instance (PrimMonad m, Variate a) => Generates (ReaderT (MWC m) m) a where
  random    = readerWrap (lift . randomPrim   )
  -- randomR r = readerWrap (lift . randomPrimR r)
