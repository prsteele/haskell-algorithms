module Algorithms.Utility where

import Control.Monad.Primitive
import qualified Data.Vector.Generic.Mutable as MG

swap :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> Int -> Int -> m ()
{-# INLINEABLE swap #-}
swap v i j = do
  ix <- MG.read v i
  jx <- MG.read v j
  MG.write v i jx
  MG.write v j ix
