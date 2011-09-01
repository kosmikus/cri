{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Random.CRI.MWC where

import Control.Monad.Primitive
import Control.Monad.Reader
import Random.CRI
import Random.CRI.Utils
import System.Random.MWC

newtype MWC m = MWC { unMWC :: Gen (PrimState m) }

instance (PrimMonad m, Variate a) => PrimSource       m MWC a where
  prandom    g = uniform    (unMWC g)

instance (PrimMonad m, Variate a) => PrimRangedSource m MWC a where
  prandomR r g = uniformR r (unMWC g)

instance (PrimMonad m, Variate a) => Source       (ReaderT (MWC m) m) a where
  random    = readerWrap (lift . prandom   )

instance (PrimMonad m, Variate a) => RangedSource (ReaderT (MWC m) m) a where
  randomR r = readerWrap (lift . prandomR r)
