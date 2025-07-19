module Algorithms.BinarySearchSpec where

import Algorithms.BinarySearch
import Algorithms.BinarySearch.Mutable qualified as BSM
import Algorithms.Sorting (quicksort)
import Algorithms.TestUtil
import Control.Monad
import Control.Monad.Primitive
import Data.Function
import Data.Maybe
import Data.Ord
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Mutable qualified as MV
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Printf

newtype SortedVec a = SortedVec (V.Vector a)
  deriving (Show, Eq)

instance (Arbitrary a, Ord a) => Arbitrary (SortedVec a) where
  arbitrary = SortedVec . quicksort . V.fromList <$> arbitrary

type BisectionBy v a = (a -> a -> Ordering) -> v a -> a -> Int

type Find v a = v a -> a -> Maybe Int

spec :: Spec
spec = do
  describe "Binary search" $ do
    describe "bisectLeft" $ do
      prop "finds a valid insertion index" $
        \(SortedVec v) x -> bisectionValid bisectLeftBy compare v (x :: Integer)
      prop "finds the leftmost insertion index" $
        \(SortedVec v) x -> checkBisectLeft v (x :: Integer)

    describe "bisectLeftBy" $
      prop "correct on reverse-sorted lists when comparing on Down" $
        \(SortedVec v) x -> bisectionValid bisectLeftBy (on compare Down) (V.reverse v) (x :: Integer)

    describe "bisectRight" $ do
      prop "finds a valid insertion index" $
        \(SortedVec v) x -> bisectionValid bisectRightBy compare v (x :: Integer)
      prop "finds the rightmost insertion index" $
        \(SortedVec v) x -> checkBisectRight v (x :: Integer)

    describe "bisectRightBy" $
      prop "correct on reverse-sorted lists when comparing on Down" $
        \(SortedVec v) x -> bisectionValid bisectRightBy (on compare Down) (V.reverse v) (x :: Integer)

    describe "findLeft" $ do
      prop "agrees with bisectionLeft when an element is found" $
        \(SortedVec v) x -> findLeftConsistent v (x :: Integer)
      prop "finds a matching element" $
        \(SortedVec v) x -> findValid findLeft v (x :: Integer)
      prop "finds the leftmost matching element" $
        \(SortedVec v) x -> checkFindLeft v (x :: Integer)

    describe "findRight" $ do
      prop "agrees with bisectRight when an element is found" $
        \(SortedVec v) x -> findRightConsistent v (x :: Integer)
      prop "finds a matching element" $
        \(SortedVec v) x -> findValid findRight v (x :: Integer)
      prop "finds the rightmost matching element" $
        \(SortedVec v) x -> checkFindRight v (x :: Integer)

    describe "bisectLeft and bisectRight" $ do
      prop "bound each other" $
        \(SortedVec v) x -> checkBounds v (x :: Integer)
      prop "bisectLeft is equivalent to bisectRight on a reversed and negated list" $
        \(SortedVec v) x -> checkBisectLeftDownReverseRight v (x :: Integer)
      prop "bisectRight is equivalent to bisectLeft on a reversed and negated list" $
        \(SortedVec v) x -> checkBisectLeftDownReverseRight v (x :: Integer)
      prop "are equal when looking for elements that don't exist" $
        \(SortedVec v) x -> checkEQMissing v x
      prop "are not equal when looking for duplicates" $
        \(SortedVec v) x -> checkNEDuplicates v x
      prop "are off by one when looking for unique element" $
        \(SortedVec v) x -> checkAdjacentUnique v x

  describe "Mutable binary search" $ do
    describe "bisectLeft" $ do
      prop "agrees with immutable bisectLeft" $
        \(SortedVec v) x -> _mutableEquivalent v (`bisectLeft` x) (`BSM.bisectLeft` (x :: Integer))
    describe "bisectLeftBy" $ do
      prop "agrees with immutable bisectLeftBy" $
        \(SortedVec v) x -> _mutableEquivalent v (\z -> bisectLeftBy compare z x) (\z -> BSM.bisectLeftBy compare z (x :: Integer))
    describe "findLeftConsistent" $ do
      prop "agrees with immutable findLeft" $
        \(SortedVec v) x -> _mutableEquivalent v (`findLeft` x) (`BSM.findLeft` (x :: Integer))
    describe "findLeftBy" $ do
      prop "agrees with immutable findLeftBy" $
        \(SortedVec v) x -> _mutableEquivalent v (\z -> findLeftBy compare z x) (\z -> BSM.findLeftBy compare z (x :: Integer))
    describe "bisectRight" $ do
      prop "agrees with immutable bisectRight" $
        \(SortedVec v) x -> _mutableEquivalent v (`bisectRight` x) (`BSM.bisectRight` (x :: Integer))
    describe "bisectRightBy" $ do
      prop "agrees with immutable bisectRightBy" $
        \(SortedVec v) x -> _mutableEquivalent v (\z -> bisectRightBy compare z x) (\z -> BSM.bisectRightBy compare z (x :: Integer))
    describe "findRightConsistent" $ do
      prop "agrees with immutable findRight" $
        \(SortedVec v) x -> _mutableEquivalent v (`findRight` x) (`BSM.findRight` (x :: Integer))
    describe "findRightBy" $ do
      prop "agrees with immutable findRightBy" $
        \(SortedVec v) x -> _mutableEquivalent v (\z -> findRightBy compare z x) (\z -> BSM.findRightBy compare z (x :: Integer))

bisectionValid :: (G.Vector v a, Ord a, PrintfArg a) => BisectionBy v a -> (a -> a -> Ordering) -> v a -> a -> Expectation
bisectionValid f cmp v x =
  let i = f cmp v x
      n = G.length v

      isLE a b = GT /= cmp a b
   in do
        when (i > 0) $
          expect2 "next-left element is no greater, but %i > %i" isLE (v G.! (i - 1)) x
        when (i < n - 1) $
          expect2 "right-right element is no lesser, but %i > %i" isLE x (v G.! (i + 1))

findLeftConsistent :: (G.Vector v a, Ord a, PrintfArg a) => v a -> a -> Expectation
findLeftConsistent v x =
  let i = bisectLeft v x
   in case findLeft v x of
        Nothing -> pure ()
        Just y -> do
          expect2 "%i == %i" (==) i y
          expect2 "%v == %v" (==) x (v G.! y)

findRightConsistent :: (G.Vector v a, Ord a, PrintfArg a) => v a -> a -> Expectation
findRightConsistent v x =
  let i = bisectRight v x
   in case findRight v x of
        Nothing -> pure ()
        Just y -> do
          expect2 "%i == %i" (==) (i - 1) y
          expect2 "%v == %v" (==) x (v G.! y)

findValid :: (G.Vector v a, Ord a, PrintfArg a) => Find v a -> v a -> a -> Expectation
findValid f v x =
  case f v x of
    Nothing -> pure ()
    Just y -> do
      expect2 "%v == %v" (==) x (v G.! y)

checkBisectLeft :: (G.Vector v a, Ord a, PrintfArg a) => v a -> a -> Expectation
checkBisectLeft v x =
  let ix = bisectLeft v x
   in do
        expect1 "%i < 0" (>= 0) ix
        expect2 "%i > %i" (<=) ix (G.length v)

        forM_ [0 .. ix - 1] $ \i -> do
          expect2 "lesser indices hold lesser values: %i >= %i" (<) (v G.! i) x

        forM_ [ix .. G.length v - 1] $ \i ->
          expect2 "other indices do not hold lesser values: %i < %i" (<=) x (v G.! i)

checkBisectRight :: (G.Vector v a, Ord a, PrintfArg a) => v a -> a -> Expectation
checkBisectRight v x =
  let ix = bisectRight v x
   in do
        expect1 "%i < 0" (>= 0) ix
        expect2 "%i > %i" (<=) ix (G.length v)

        forM_ [ix .. G.length v - 1] $ \i ->
          expect2 "greater indices hold greater values: %i <= %i" (<) x (v G.! i)

        forM_ [0 .. ix - 1] $ \i -> do
          expect2 "other indices do not hold greater values: %i > %i" (<=) (v G.! i) x

checkFindLeft :: (G.Vector v a, Ord a, PrintfArg a) => v a -> a -> Expectation
checkFindLeft v x =
  case findLeft v x of
    Nothing -> forM_ [0 .. G.length v - 1] $ \i ->
      expect2 "%i is not in the vector: found %i" (/=) x (v G.! i)
    Just i -> do
      expect2 "%i is in the vector: found %i" (==) x (v G.! i)
      when (i > 0) $
        expect2 "should have found leftmost: %i == %i" (/=) x (v G.! (i - 1))

checkFindRight :: (G.Vector v a, Ord a, PrintfArg a) => v a -> a -> Expectation
checkFindRight v x =
  case findRight v x of
    Nothing -> forM_ [0 .. G.length v - 1] $ \i ->
      expect2 "%i is not in the vector: found %i" (/=) x (v G.! i)
    Just i -> do
      expect2 "%i is in the vector: found %i" (==) x (v G.! i)
      when (i < G.length v - 1) $
        expect2 "should have found rightmost: %i == %i" (/=) x (v G.! (i + 1))

checkBounds :: (G.Vector v a, Ord a, PrintfArg a) => v a -> a -> Bool
checkBounds v x = bisectLeft v x <= bisectRight v x

checkBisectLeftDownReverseRight :: (Ord a, PrintfArg a) => V.Vector a -> a -> Bool
checkBisectLeftDownReverseRight v x =
  let l = bisectLeft v x
      r = bisectRight (fmap Down (G.reverse v)) (Down x)
   in l == G.length v - r

checkBisectRightDownReverseLeft :: (Ord a, PrintfArg a) => V.Vector a -> a -> Bool
checkBisectRightDownReverseLeft v x =
  let l = bisectLeft (fmap Down (G.reverse v)) (Down x)
      r = bisectRight v x
   in l == G.length v - r

checkAdjacentUnique :: V.Vector Integer -> Integer -> Expectation
checkAdjacentUnique v' x =
  let v = quicksort . V.cons x . V.filter (/= x) $ v'
      l = bisectLeft v x
      r = bisectRight v x
   in do
        expect2 "bisectLeft should be one less than bisectRight: %i /= %i - 1" (==) l (r - 1)
        expect "findLeft finds the index of bisectLeft" (Just l == findLeft v x)
        expect "findRight finds the (adjusted) index of bisectRight" (Just (r - 1) == findRight v x)

checkEQMissing :: V.Vector Integer -> Integer -> Expectation
checkEQMissing v' x =
  let v = V.filter (/= x) v'
      l = bisectLeft v x
      r = bisectRight v x
   in do
        expect2 "bisectLeft should equal bisectRight: %i /= %i" (==) l r
        expect "findLeft finds Nothing" (isNothing (findLeft v x))
        expect "findRight finds Nothing" (isNothing (findRight v x))

checkNEDuplicates :: V.Vector Integer -> Integer -> Expectation
checkNEDuplicates v' x =
  let v = quicksort (V.fromList [x, x] <> v')
      l = bisectLeft v x
      r = bisectRight v x
   in do
        expect2 "bisectLeft should differ from bisectRight: %i == %i" (/=) l r
        expect "findLeft finds the index of bisectLeft" (Just l == findLeft v x)
        expect "findRight finds the (adjusted) index of bisectRight" (Just (r - 1) == findRight v x)

_mutableEquivalent :: (Show b, Eq b) => V.Vector a -> (V.Vector a -> b) -> (MV.MVector RealWorld a -> IO b) -> Expectation
_mutableEquivalent v f g = do
  mv <- V.thaw v
  g mv `shouldReturn` f v
