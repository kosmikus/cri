{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, FlexibleContexts #-}
module Random.CRI.Base where

import Control.Monad.Identity
import Control.Monad.ST

class RNG g m where
  type Initializer g :: *
  type Snapshot g :: *
  initM    :: Initializer g -> m g
  saveM    :: g -> m (Snapshot g)
  restoreM :: Snapshot g -> m ()

class RNG g m => Splittable g m where
  type SplitInitalizer g :: *
  splitInitM :: Int -> g -> m [g]

{-# INLINE initIO #-}
initIO :: (RNG g IO) => Initializer g -> IO g
initIO = initM

{-# INLINE initST #-}
initST :: (RNG g (ST s)) => Initializer g -> ST s g
initST = initM

{-# INLINE init #-}
init :: (RNG g Identity) => Initializer g -> g
init = runIdentity . initM

class RNG g m => Generates g a m where
  nextM    :: g -> m (a, g)

next :: (Generates g a Identity) => g -> (a, g)
next g = runIdentity (nextM g)
