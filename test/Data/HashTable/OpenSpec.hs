module Data.HashTable.OpenSpec where

import Algorithms.TestUtil
import Control.Monad
import qualified Data.HashTable.Open as H
import Data.Hashable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  describe "Insertion" $ do
    prop "Just v == insert k v >> lookup k" insertLookupProp

insertLookupProp :: [(Int, Int)] -> Property
insertLookupProp kvs = monadicIO $ do
  t <- run (H.new hash H.linear 0)
  forM_ kvs $ \(k, v) -> do
    run $ H.insert t k v
    mv <- run (H.lookup t k)
    mv ==? Just v
