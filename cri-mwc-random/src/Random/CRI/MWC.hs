{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Random.CRI.MWC where

import Control.Monad
import Random.CRI
import qualified System.Random.MWC as R

newtype MWC s = MWC (R.GenST s)

instance Source MWC R.Seed where
  randomST (MWC r) = R.uniform r
  save     (MWC r) = R.save r
  restore  g       = liftM MWC (R.restore g)
