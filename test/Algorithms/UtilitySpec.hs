module Algorithms.UtilitySpec where

import Algorithms.TestUtil
import Algorithms.Utility
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Poly

spec :: Spec
spec = do
  describe "sequences" $ do
    prop "upTo == reverse . flip downTo" $ \i j ->
      (i `upTo` j) == reverse (j `downTo` (i :: Int))
    prop "all (between i j) (upTo i j)" $
      recombine (recombine all) between (upTo @Int)
  describe "combinators" $ do
    prop "recombine f g h x == f (g x) (h x)" recombineDef

recombineDef :: Fun (B, C) D -> Fun A B -> Fun A C -> A -> Bool
recombineDef (Fun _ f') (Fun _ g) (Fun _ h) x = recombine f g h x == f (g x) (h x)
  where
    f = curry f'
