{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FunctionalDependencies #-}
module Random.CRI.Source where

import Control.Monad.ST
import Data.Word

-- Assumptions:
--
-- The RNG produces bits. So it produces just
-- (Word32 and) Word64 values. All other values
-- are obtained by conversion from these.
--
-- For temporary simplicity, we assume 64 bits.
--
-- For most efficient storage, we assume that
-- it might need to use the ST monad.

-- | The Source class takes as parameters the type of RNG state references r,
-- and the type of RNG state snapshots g.

class Source r g | r -> g, g -> r where

  -- random32 :: g -> (Word32, g)
  random :: g -> (Word64, g)
  random g = run g randomST

  -- random32ST :: r s -> ST s Word32
  randomST :: r s -> ST s Word64

  save    :: r s -> ST s g
  restore :: g -> ST s (r s)

  run :: g -> (forall s. r s -> ST s a) -> (a, g)
  run g x =
    runST (do
             r  <- restore g
             a  <- x r
             g' <- save r
             return (a, g'))

