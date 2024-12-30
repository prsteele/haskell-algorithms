-- | Sort triples.
--
-- This is mostly useful as a subroutine; for example, this can be
-- used to implement a median-of-3 pivot rule for
-- 'Algorithms.Sorting.quickSort'.
--
-- == Complexity
--
-- This runs in \(O(1)\) time, performing exactly three
-- compare-and-swap operations.
--
-- == References
--
-- This is a special case of the more general
-- https://en.wikipedia.org/wiki/Sorting_network problem.
module Algorithms.Sorting.Sort3
  ( sort3,
    sort3By,
    sort3On,
  )
where

import Data.Ord (comparing)

sort3 :: (Ord a) => a -> a -> a -> (a, a, a)
{-# INLINEABLE sort3 #-}
sort3 = sort3By compare

sort3On :: (Ord b) => (a -> b) -> a -> a -> a -> (a, a, a)
{-# INLINEABLE sort3On #-}
sort3On f x y z =
  let tag a = let b = f a in b `seq` (b, a)
      ((_, x'), (_, y'), (_, z')) = sort3By (comparing fst) (tag x) (tag y) (tag z)
   in (x', y', z')

sort3By :: (a -> a -> Ordering) -> a -> a -> a -> (a, a, a)
{-# INLINEABLE sort3By #-}
sort3By cmp x0 y0 z0 =
  let cswap x y
        | cmp x y /= GT = (x, y)
        | otherwise = (y, x)

      ((x1, y1), z1) = (cswap x0 y0, z0) -- x1 <= y1
      (x2, (y2, z2)) = (x1, cswap y1 z1) -- y2 <= z2, x2 <= z2
      ((x3, y3), z3) = (cswap x2 y2, z2) -- x3 <= y3 <= z3
   in (x3, y3, z3)
