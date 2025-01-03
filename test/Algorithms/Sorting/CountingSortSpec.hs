module Algorithms.Sorting.CountingSortSpec where

import Algorithms.Sorting.CountingSort
import Algorithms.Sorting.TestUtil
import Algorithms.TestUtil
import Data.Foldable (Foldable (foldl'))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "countingSort" $
    do
      prop "is a stable sorting algorithm" $ \(IntVector v) -> do
        -- We inline isStableSortingAlgorithm to avoid problems with
        -- rank-2 type inference
        let v' = V.map (uncurry Ix) (V.indexed v)
        isSortingAlgorithm countingSortAdapter v
        isStablySorted (countingSortAdapter v')

countingSortAdapter :: (Show a, Integral a, G.Vector v a) => v a -> v a
countingSortAdapter v =
  let range =
        if G.null v
          then (0, 0)
          else bounds (G.toList v)
   in case countingSort range v of
        Left x -> error ("countingSort encountered an invalid valud " <> show x)
        Right result -> result

bounds :: (Ord a) => [a] -> (a, a)
bounds (x : xs) = foldl' f (x, x) xs
  where
    f (lo, hi) z = (min lo z, max hi z)
bounds _ = error "Empty list"
