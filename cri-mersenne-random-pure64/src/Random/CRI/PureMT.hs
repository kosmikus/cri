{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.PureMT where

import Random.CRI
import Random.CRI.Utils
import qualified System.Random.Mersenne.Pure64 as R

instance Source (R R.PureMT) R.PureMT where
  random   = R.randomWord64
  randomST = dRandomST
  save     = dSave
  restore  = dRestore
