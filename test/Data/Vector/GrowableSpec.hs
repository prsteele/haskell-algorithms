module Data.Vector.GrowableSpec where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Growable as GV
import Test.Hspec

spec :: Spec
spec =
  describe "GrowVectors" $ do
    it "can grow" $ do
      gv <- GV.new 10
      forM_ [1 .. 100 :: Int] $ \i -> do
        GV.append gv i
        v <- GV.mvector gv >>= V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can grow from an empty vector" $ do
      gv <- GV.new 0
      forM_ [1 .. 100 :: Int] $ \i -> do
        GV.append gv i
        v <- GV.mvector gv >>= V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can augment existing MVectors" $ do
      mv <- V.thaw (V.fromList [1 .. 20 :: Int])
      gv <- GV.fromMVector mv
      forM_ [21 .. 100] $ \i -> do
        GV.append gv i
        v <- GV.mvector gv >>= V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can augment existing Vectors" $ do
      gv <- GV.fromVector (V.fromList [1 .. 20 :: Int])
      forM_ [21 .. 100] $ \i -> do
        GV.append gv i
        v <- GV.mvector gv >>= V.freeze
        v `shouldBe` V.fromList [1 .. i]
    it "can be shrunk" $ do
      gv <- GV.fromVector (V.fromList [1 .. 20 :: Int])
      GV.shrink gv 10
      v <- GV.mvector gv >>= V.freeze
      v `shouldBe` V.fromList [1 .. 10]
