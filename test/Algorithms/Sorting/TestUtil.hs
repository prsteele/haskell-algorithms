{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Algorithms.Sorting.TestUtil where

import Algorithms.Permutations
import Control.Monad
import Data.Function
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Lens.Micro hiding (ix)
import Lens.Micro.TH
import Test.HUnit
import Test.Hspec

-- | A value tagged by its index in a sequence.
--
-- We prodide 'Num' and related instances that use 'undefined' for all
-- new indices; this is to allow this type to be used with e.g. Radix
-- sort. No actual sorting algorithm should need arithmetic.
data Ix a = Ix {_ix :: Int, _value :: a}
  deriving (Show, Eq)

makeLenses ''Ix

instance Ord a => Ord (Ix a) where
  compare = compare `on` (^. value)

-- To help out some algorithms, we provide a correspondence to the
-- integers. We leave all tags as bottom, as the only actual operation
-- that should be used by sorting algorithms is toInteger.
instance Num a => Num (Ix a) where
  Ix _ x + Ix _ y = Ix undefined (x + y)
  Ix _ x * Ix _ y = Ix undefined (x * y)
  abs (Ix _ x) = Ix undefined (abs x)
  signum (Ix _ x) = Ix undefined x
  fromInteger = Ix undefined . fromInteger
  negate (Ix _ x) = Ix undefined (negate x)

instance Enum a => Enum (Ix a) where
  toEnum = Ix undefined . toEnum
  fromEnum (Ix _ x) = fromEnum x

instance Real a => Real (Ix a) where
  toRational (Ix _ x) = toRational x

instance Integral a => Integral (Ix a) where
  quotRem (Ix _ x) (Ix _ y) = (Ix undefined (x `quot` y), Ix undefined (x `rem` y))
  toInteger (Ix _ x) = fromIntegral x

newtype WithIx a = WithIx {unWithIx :: Ix a}
  deriving (Show, Eq)

instance Ord a => Ord (WithIx a) where
  compare (WithIx x) (WithIx y) = compare x y <> (compare `on` (^. ix)) x y

mkIxs :: [a] -> [Ix a]
mkIxs = zipWith Ix [0 ..]

-- | Verify that a sorted list of 'Ix a' has maintained index ordering
-- in the case of ties.
isStablySorted :: (Show a, Ord a, G.Vector v (WithIx a), G.Vector v (Ix a)) => v (Ix a) -> Expectation
isStablySorted v = do
  isSorted (G.map WithIx v)

-- | Verify that the given function is a valid sorting algorithm.
--
-- We simply compare the result
isSortingAlgorithm :: (Show a, Ord a, G.Vector v a) => (v a -> v a) -> v a -> Expectation
isSortingAlgorithm f v = f v `isSortedCopyOf` v

-- | Verify that the given function is a valid stable sorting algorithm.
--
-- This implies 'isSortingAlgorithm'.
isStableSortingAlgorithm ::
  (Show a, Ord a, G.Vector v a, G.Vector v (Ix a), G.Vector v (WithIx a), G.Vector v (Int, a)) =>
  (forall b. (Ord b, G.Vector v b) => v b -> v b) ->
  v a ->
  Expectation
isStableSortingAlgorithm f v = do
  isSortingAlgorithm f v
  isStablySorted (f v')
  where
    v' = G.map (uncurry Ix) (G.indexed v)

isSortedCopyOf :: (Show a, Ord a, G.Vector v a) => v a -> v a -> Expectation
isSortedCopyOf sorted unsorted = do
  -- Verify the sorted list is, in fact, sorted
  isSorted sorted

  -- Verify the sorted list is a permutation of the second
  case (sorted `permutationTo` unsorted) :: Maybe (V.Vector Int) of
    Just _ -> pure ()
    Nothing ->
      assertFailure
        ( "sorting the list did not produce a permutation: "
            <> show (G.toList sorted)
        )

isSorted :: (Show a, G.Vector v a, Ord a) => v a -> Expectation
isSorted xs = forM_ (zip (G.toList xs) (tail (G.toList xs))) $ \(x, y) -> do
  when (x > y) $ do
    assertFailure (show x <> " > " <> show y <> " in " <> show (G.toList xs))
