module Algorithms.Sorting.TestUtil where

import Control.Monad
import Data.Foldable
import Data.List
import Test.HUnit
import Test.Hspec

assertIsSortedCopyOf :: (Show a, Ord a, Foldable f) => f a -> f a -> Expectation
assertIsSortedCopyOf sorted unsorted = do
  assertSorted sorted
  when (toList sorted /= sort (toList unsorted)) $ do
    assertFailure
      ( "elements of " <> show (toList sorted)
          <> " differ from "
          <> show (toList unsorted)
      )

assertSorted :: (Show a, Foldable f, Ord a) => f a -> Expectation
assertSorted xs = forM_ (zip (toList xs) (tail (toList xs))) $ \(x, y) -> do
  when (x > y) $ do
    assertFailure (show x <> " > " <> show y <> " in " <> show (toList xs))
