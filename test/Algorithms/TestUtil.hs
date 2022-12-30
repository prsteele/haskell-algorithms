module Algorithms.TestUtil where

import qualified Data.Vector as V
import Test.QuickCheck

newtype IntVector = IntVector (V.Vector Int)
  deriving (Show)

instance Arbitrary IntVector where
  arbitrary = fmap (IntVector . V.fromList) arbitrary

newtype IntegerVector = IntegerVector (V.Vector Integer)
  deriving (Show)

instance Arbitrary IntegerVector where
  arbitrary = fmap (IntegerVector . V.fromList) arbitrary
