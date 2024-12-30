{-# LANGUAGE LambdaCase #-}

module Data.Vector.GrowableSpec where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Growable as GV
import qualified Data.Vector.Growable.Generic as GG
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec =
  describe "GrowVectors" $ do
    it "can grow" $ do
      gv <- GV.empty 10
      forM_ [1 .. 100 :: Int] $ \i -> do
        GV.append gv i
        v <- GV.withMVector gv V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can grow from an empty vector" $ do
      gv <- GV.empty 0
      forM_ [1 .. 100 :: Int] $ \i -> do
        GV.append gv i
        v <- GV.withMVector gv V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can augment existing MVectors" $ do
      mv <- V.thaw (V.fromList [1 .. 20 :: Int])
      gv <- GV.fromMVector mv
      forM_ [21 .. 100] $ \i -> do
        GV.append gv i
        v <- GV.withMVector gv V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can augment existing Vectors" $ do
      gv <- GV.fromVector (V.fromList [1 .. 20 :: Int])
      forM_ [21 .. 100] $ \i -> do
        GV.append gv i
        v <- GV.withMVector gv V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can be shrunk" $ do
      gv <- GV.fromVector (V.fromList [1 .. 20 :: Int])
      GV.shrink gv 10
      v <- GV.withMVector gv V.freeze
      v `shouldBe` V.fromList [1 .. 10]
    prop "behaves correctly" $ \initState actions -> do
      let expected = evalActionsList initState actions :: [Int]
      gv <- V.thaw (V.fromList initState) >>= GV.fromMVector
      evalActions gv actions
      result <- GV.withMVector gv $ fmap V.toList . V.freeze
      result `shouldBe` expected

data GrowCommand = DoAppend | DoReserve | DoConserve | DoShrink | DoCapacity | DoMutate
  deriving (Show, Enum, Bounded)

instance Arbitrary GrowCommand where
  arbitrary = arbitraryBoundedEnum

data GrowAction a = Append a | Reserve (NonNegative Int) | Conserve | Shrink (NonNegative Int) | Capacity | Mutate
  deriving (Show)

newtype GrowActions a = GrowActions [GrowAction a]
  deriving (Show)

instance (Arbitrary a) => Arbitrary (GrowAction a) where
  arbitrary =
    arbitrary >>= \case
      DoAppend -> fmap Append arbitrary
      DoReserve -> fmap Reserve arbitrary
      DoConserve -> pure Conserve
      DoShrink -> fmap Shrink arbitrary
      DoCapacity -> pure Capacity
      DoMutate -> pure Mutate

instance (Arbitrary a) => Arbitrary (GrowActions a) where
  arbitrary = fmap GrowActions arbitrary

evalActions :: (PrimMonad m, GG.GrowVector v a) => v (PrimState m) a -> [GrowAction a] -> m ()
evalActions gv = mapM_ eval
  where
    eval (Append x) = GG.append gv x
    eval (Reserve (NonNegative i)) = GG.reserve gv i
    eval Conserve = GG.conserve gv
    eval (Shrink (NonNegative i)) = GG.shrink gv i
    eval Capacity = void (GG.capacity gv)
    eval Mutate = GG.withMVector gv $ \mv -> do
      let n = MG.length mv
      when (n > 0) $ do
        x <- MG.read mv 0
        forM_ [1 .. n - 1] $ \i -> do
          MG.read mv i >>= MG.write mv (i - 1)
        MG.write mv (n - 1) x

evalActionsList :: [a] -> [GrowAction a] -> [a]
evalActionsList = foldl eval
  where
    eval xs (Append x) = xs ++ [x]
    eval xs (Reserve _) = xs
    eval xs Conserve = xs
    eval xs (Shrink (NonNegative i)) = take i xs
    eval xs Capacity = xs
    eval [] Mutate = []
    eval (x : xs) Mutate = xs ++ [x]
