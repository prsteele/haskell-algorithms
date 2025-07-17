{-# LANGUAGE DerivingVia #-}

module Algorithms.TestUtil where

import Data.Vector qualified as V
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Poly
import Text.Printf

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

(==?) :: (Monad m, Show a, Eq a) => a -> a -> PropertyM m ()
x ==? y =
  if x == y
    then assertWith True (show x <> " == " <> show y)
    else assertWith False (show x <> " /= " <> show y)

expect :: String -> Bool -> Expectation
expect msg x
  | x = pure ()
  | otherwise = expectationFailure msg

expect1 :: (PrintfArg a) => String -> (a -> Bool) -> a -> Expectation
expect1 msg f x
  | f x = pure ()
  | otherwise = expectationFailure (printf msg x)

expect2 :: (PrintfArg a, PrintfArg b) => String -> (a -> b -> Bool) -> a -> b -> Expectation
expect2 msg f x y
  | f x y = pure ()
  | otherwise = expectationFailure (printf msg x y)
