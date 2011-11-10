{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, KindSignatures #-}
module Random.CRI.SPRNG where

import Control.Monad.Reader
import Random.CRI
import Random.CRI.Utils
import qualified System.Random.SPRNG.LFG as R

newtype SPRNG (m :: * -> *) = SPRNG { unSPRNG :: R.Gen }

instance (R.Variate a) => Source       IO SPRNG a where
  grandom    g = R.uniform    (unSPRNG g)

instance (R.Variate a) => RangedSource IO SPRNG a where
  grandomR r g = R.uniformR r (unSPRNG g)

instance (R.Variate a) => Source       (ReaderT (SPRNG IO) IO) ImpliedGen a where
  grandom    _ = readerWrap (lift . grandom   )

instance (R.Variate a) => RangedSource (ReaderT (SPRNG IO) IO) ImpliedGen a where
  grandomR r _ = readerWrap (lift . grandomR r)

create :: Int -> IO (SPRNG IO)
create s = liftM SPRNG (R.create s)

spawn :: SPRNG IO -> Int -> IO [SPRNG IO]
spawn (SPRNG g) n = liftM (map SPRNG) (R.spawn g n)
