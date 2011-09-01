{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Random.CRI.MWC where

import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Vector.Generic
import Data.Word
import Random.CRI
import Random.CRI.Utils
import qualified System.Random.MWC as R

newtype MWC m = MWC { unMWC :: R.Gen (PrimState m) }

instance (PrimMonad m, R.Variate a) => Source       m MWC a where
  grandom    g = R.uniform    (unMWC g)

instance (PrimMonad m, R.Variate a) => RangedSource m MWC a where
  grandomR r g = R.uniformR r (unMWC g)

instance (PrimMonad m, R.Variate a) => Source       (ReaderT (MWC m) m) ImpliedGen a where
  grandom    _ = readerWrap (lift . grandom   )

instance (PrimMonad m, R.Variate a) => RangedSource (ReaderT (MWC m) m) ImpliedGen a where
  grandomR r _ = readerWrap (lift . grandomR r)

create :: (PrimMonad m) => m (MWC m)
create = liftM MWC R.create

initialize :: (PrimMonad m, Vector v Word32) => v Word32 -> m (MWC m)
initialize = liftM MWC . R.initialize

