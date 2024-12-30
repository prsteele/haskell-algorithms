module Algorithms.Utility where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | A closed ascending sequence.
upTo :: (Integral a) => a -> a -> [a]
i `upTo` j = [i .. j]

-- | A closed descending sequence.
downTo :: (Integral a) => a -> a -> [a]
i `downTo` j = [i, i - 1 .. j]

between :: (Ord a) => a -> a -> a -> Bool
between lo hi x = lo <= x && x <= hi

-- | Assign a vector to the slice of another.
--
-- It must be that @offset + G.length v < MG.length output@.
assignSlice ::
  (PrimMonad m, MG.MVector v a, G.Vector w a) =>
  -- | The offset to begin writing to the output
  Int ->
  -- | The vector to read from
  w a ->
  -- | The vector to write to
  v (PrimState m) a ->
  m ()
assignSlice offset v output =
  let lenV = G.length v
   in forM_ (0 `upTo` pred lenV) $ \i -> do
        MG.write output (offset + i) (v G.! i)

-- | Apply a function to the result of two functions.
--
-- Examples:
--
-- >>> mean = recombine (/) sum (fromIntegral . length)
-- >>> mean [1, 2, 3]
-- 2.0
--
-- >>> isPalindrome = recombine (==) id reverse
-- >>> isPalindrome [1, 2, 3]
-- False
-- >>> isPalindrome [1, 2, 3, 2, 1]
-- True
recombine :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
recombine f g h x = f (g x) (h x)

-- | Duplicate the argument to a function.
--
-- Examples:
--
-- >>> square = dup (*)
-- >>> square 3
-- 9
dup :: (a -> a -> b) -> a -> b
dup f x = f x x
