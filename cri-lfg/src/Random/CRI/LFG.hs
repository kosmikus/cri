{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.LFG where

import Control.Monad.Primitive
import Control.Monad.State
import Data.StateRef
import Random.CRI
import Random.CRI.Utils
import qualified Random.LFG as R

instance Monad m => Source (StateT R.Gen m) ImpliedGen Double where
  grandom _ = stateWrap R.step

newtype LFG     m = LFG   { unLFG   :: Ref m R.Gen       }
newtype LFGST s m = LFGST { unLFGST :: Ref m (R.GenST s) }

instance (PrimMonad m) => Source m LFG Double where
  grandom (LFG p) = primWrap R.step p

-- TODO: extend to IO
instance Source (ST s) (LFGST s) Double where
  grandom (LFGST p) = do
    g       <- readRef p
    (x, g') <- R.stepST g
    writeRef p g'
    return x

initST :: Int -> Int -> [Double] -> ST s (LFGST s (ST s))
initST m n xs = liftM LFGST (R.initST m n xs >>= newRef)
