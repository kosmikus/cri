module Random.CRI.Utils where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Word

import Random.CRI.Source

-- newtype G32 = G32 [Word32]
-- newtype G r = G [Word64] -- TODO: use a stream

newtype R g s = R (STRef s g)

dRandomST :: (Source r g) => R g s -> ST s Word64
dRandomST (R r) = do
                    g <- readSTRef r
                    let (x, g') = random g
                    writeSTRef r g'
                    return x

dSave :: R g s -> ST s g
dSave (R r) = readSTRef r

dRestore :: g -> ST s (R g s)
dRestore g = liftM R (newSTRef g)
