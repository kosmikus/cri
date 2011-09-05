{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Random.CRI.StdGen where

import Random.CRI
import Random.CRI.Utils
import qualified System.Random as R

instance Source (R R.StdGen) R.StdGen where
  -- TODO: check if system is 32-bit or 64-bit, and convert accordingly
  random g = let (x, g') = R.next g in (fromIntegral x, g')
  randomST = dRandomST
  save     = dSave
  restore  = dRestore
