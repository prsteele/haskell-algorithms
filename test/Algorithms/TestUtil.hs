{-# LANGUAGE DerivingVia #-}

module Algorithms.TestUtil where

import qualified Data.Vector as V
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Poly

newtype IntVector = IntVector (V.Vector Int)
  deriving (Show)

instance Arbitrary IntVector where
  arbitrary = fmap (IntVector . V.fromList) arbitrary

newtype IntegerVector = IntegerVector (V.Vector Integer)
  deriving (Show)

instance Arbitrary IntegerVector where
  arbitrary = fmap (IntegerVector . V.fromList) arbitrary

-- | A newtype wrapper around 'Integer', extending 'A', 'B', and 'C'
-- from 'QuickCheck.Poly'.
newtype D = D {unD :: Integer}
  deriving (Eq, Show, Arbitrary, CoArbitrary) via A

failure :: (Monad m) => String -> PropertyM m ()
failure = assertWith False
