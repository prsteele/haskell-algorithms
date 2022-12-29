{-# OPTIONS_GHC -Wno-orphans #-}

module Algorithms.TestUtil where

import qualified Data.Vector as V
import Test.QuickCheck

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary
