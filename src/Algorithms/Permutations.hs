{-# LANGUAGE FlexibleContexts #-}

-- | Determining whether two vectors are permutations of each other.
--
-- == Complexity
--
-- This algorithm runs in \(O(n \log n)\) time, since each list is sorted.
module Algorithms.Permutations where

import Algorithms.Sorting (mergeSort)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import Prelude hiding (pi)

-- | Is one vector a permutation of another?
--
-- The mnemonic for this function is that if @x `permutationTo` y@
-- produces @Just pi@, then @G.backpermute x pi == y@.
--
-- Returns 'Just' a permutation vector transforming the first vector
-- into the second when the vectors are permutations of each other,
-- and 'Nothing' otherwise.
permutationTo :: (G.Vector v a, G.Vector w Int, Ord a) => v a -> v a -> Maybe (w Int)
permutationTo u v =
  let sortedU = sortWithIndices u
      sortedV = sortWithIndices v

      indices = G.map fst
      values = G.map snd

      sameLength = G.length u == G.length v
      sameContents = values sortedU == values sortedV

      indexPairs = zip (V.toList (indices sortedV)) (V.toList (indices sortedU))
   in if not (sameLength && sameContents)
        then Nothing
        else runST $ do
          pi <- MG.new (G.length u)
          forM_ indexPairs $ uncurry (MG.write pi)
          fmap Just (G.freeze pi)

newtype By2 a b = By2 {unBy2 :: (a, b)}
  deriving
    ( Eq
    )

instance (Ord a, Ord b) => Ord (By2 a b) where
  compare (By2 (x, y)) (By2 (x', y')) = compare y y' <> compare x x'

sortWithIndices :: (G.Vector v a, Ord a) => v a -> V.Vector (Int, a)
sortWithIndices = V.map (.unBy2) . mergeSort . V.map By2 . V.indexed . G.convert
