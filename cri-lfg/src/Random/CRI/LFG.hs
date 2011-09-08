{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.LFG where

import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Word
import Random.CRI
import Random.CRI.Utils
import qualified System.Random.LFG as R

newtype LFG m = LFG { unLFG :: R.Gen (PrimState m) }

instance (PrimMonad m, R.Variate a) => Source       m LFG a where
  grandom    g = R.uniform    (unLFG g)

instance (PrimMonad m, R.Variate a) => RangedSource m LFG a where
  grandomR r g = R.uniformR r (unLFG g)

instance (PrimMonad m, R.Variate a) => Source       (ReaderT (LFG m) m) ImpliedGen a where
  grandom    _ = readerWrap (lift . grandom   )

instance (PrimMonad m, R.Variate a) => RangedSource (ReaderT (LFG m) m) ImpliedGen a where
  grandomR r _ = readerWrap (lift . grandomR r)

create :: (PrimMonad m) => R.Lags -> Int -> [Word32] -> m [LFG m]
create l n xs = liftM (map LFG) (R.create l n xs)
