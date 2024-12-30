-- | Algorithms to shuffle vectors.
--
-- These shuffles are pseudo-random shuffles; please don't use them
-- for anything that requires true randomness.
--
-- == Complexity
--
-- This algorithm runs in \(\Theta(n)\), where \(n\) is the length of
-- the vector.
--
-- == References
--
-- This is a Fisher-Yates shuffle; see
-- [here](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle).
module Algorithms.Shuffle where

import Algorithms.Utility
import Control.Monad
import Control.Monad.Primitive (PrimMonad (PrimState))
import Control.Monad.ST
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import System.Random
import System.Random.Stateful

-- | Shuffle a vector, using the given seed for random number
-- generation.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | No         |
-- +------------+------------+
shuffle :: G.Vector v a => Int -> v a -> v a
shuffle seed v = runST $ do
  mv <- G.thaw v
  gen <- newSTGenM (mkStdGen seed)
  mutShuffle gen mv
  G.freeze mv

-- | Shuffle a vector, using a global random number generator.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | No         |
-- +------------+------------+
shuffleIO :: G.Vector v a => v a -> IO (v a)
shuffleIO v = do
  mv <- G.thaw v
  mutShuffle globalStdGen mv
  G.freeze mv

-- | An in-place shuffle, using the provided random number generator.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | Yes        |
-- +------------+------------+
--
-- See 'shuffle'.
mutShuffle :: (StatefulGen g m, PrimMonad m, MG.MVector v a) => g -> v (PrimState m) a -> m ()
mutShuffle g v = do
  forM_ ((MG.length v - 1) `downTo` 1) $ \i -> do
    j <- uniformRM (0, i) g
    swap v i j

-- | An in-place shuffle, using a global random number generator.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | Yes        |
-- +------------+------------+
mutShuffleIO :: MG.MVector v a => v (PrimState IO) a -> IO ()
mutShuffleIO = mutShuffle globalStdGen
