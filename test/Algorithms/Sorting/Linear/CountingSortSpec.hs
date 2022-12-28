{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Algorithms.Sorting.Linear.CountingSortSpec where

import Algorithms.Sorting.Linear.CountingSort
import Algorithms.Sorting.TestUtil
import Data.Foldable (Foldable (foldl'))
import Data.Function (on)
import qualified Data.Vector as V
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  describe "CountingSort" $ do
    prop "sorts arbitrary lists" $ runAssertions . V.fromList . fmap (getSmall :: Small Int -> Int)
    prop "sorts arbitrary lists of tagged values" $ runAssertions . tagNums

runAssertions :: (Show a, Integral a) => V.Vector a -> Expectation
runAssertions v =
  let Extrema (lo, hi) = case V.toList v of
        (x : xs) -> bounds x xs
        _ -> Extrema (0, 0)
   in case countingSort (lo, hi) v of
        Left x ->
          assertFailure
            ( "Test assumption failed, found value "
                <> show x
                <> " outside "
                <> show (lo, hi)
            )
        Right sorted -> sorted `assertIsSortedCopyOf` v

-- Numbers tagged with an identifier. We provide definitions for
-- Integral that do not preserve the identifiers.
newtype Tagged = Tagged (Int, Int)
  deriving (Show, Eq)

tagNums :: [Int] -> V.Vector Tagged
tagNums = V.fromList . fmap Tagged . zip [1 ..]

taggedVal :: Tagged -> Int
taggedVal (Tagged (_, x)) = x

meaningless :: Int -> Tagged
meaningless = Tagged . (0,)

binOp :: (Int -> Int -> Int) -> Tagged -> Tagged -> Tagged
binOp op x y = meaningless ((op `on` taggedVal) x y)

instance Num Tagged where
  (+) :: Tagged -> Tagged -> Tagged
  (+) = binOp (+)
  (*) :: Tagged -> Tagged -> Tagged
  (*) = binOp (*)
  abs :: Tagged -> Tagged
  abs = meaningless . abs . taggedVal
  signum :: Tagged -> Tagged
  signum = meaningless . signum . taggedVal
  fromInteger :: Integer -> Tagged
  fromInteger = meaningless . fromInteger
  negate :: Tagged -> Tagged
  negate = meaningless . negate . taggedVal

instance Real Tagged where
  toRational = toRational . taggedVal

instance Enum Tagged where
  fromEnum = taggedVal
  toEnum x = Tagged (0, x)

instance Integral Tagged where
  quotRem :: Tagged -> Tagged -> (Tagged, Tagged)
  quotRem x y = (Tagged (0, taggedVal x `quot` taggedVal y), Tagged (0, taggedVal x `rem` taggedVal y))

  toInteger :: Tagged -> Integer
  toInteger = fromIntegral . taggedVal

instance Ord Tagged where
  compare = compare `on` taggedVal

newtype Extrema a = Extrema (a, a)
  deriving (Show)

instance Ord a => Semigroup (Extrema a) where
  (Extrema (lo, hi)) <> (Extrema (lo', hi')) = Extrema (min lo lo', max hi hi')

bounds :: Ord a => a -> [a] -> Extrema a
bounds x xs = foldl' (<>) (single x) (fmap single xs)
  where
    single z = Extrema (z, z)
