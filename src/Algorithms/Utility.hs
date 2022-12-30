module Algorithms.Utility where

import Algorithms.Sequence
import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | Swap the contents of two indices.
swap :: (PrimMonad m, MG.MVector v a) => v (PrimState m) a -> Int -> Int -> m ()
{-# INLINEABLE swap #-}
swap v i j = do
  ix <- MG.read v i
  jx <- MG.read v j
  MG.write v i jx
  MG.write v j ix

-- | Assign a vector to the slice of another.
--
-- It must be that @offset + G.length v < MG.length output@.
assignSlice ::
  (PrimMonad m, MG.MVector v a, G.Vector w a) =>
  -- ^ The offset to begin writing to the output
  Int ->
  -- ^ The vector to read from
  w a ->
  -- ^ The vector to write to
  v (PrimState m) a ->
  m ()
assignSlice offset v output =
  let lenV = G.length v
   in forM_ (0 `upTo` pred lenV) $ \i -> do
        MG.write output (offset + i) (v G.! i)
