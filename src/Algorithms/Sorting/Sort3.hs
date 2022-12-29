module Algorithms.Sorting.Sort3 where

-- | Sort triples.
--
-- This is mostly useful as a subroutine; for example, this can be
-- used to implement a median-of-3 pivot rule for
-- 'Algorithms.Sorting.quickSort'.
--
-- +------------+------------+
-- | Attribute  |            |
-- +============+============+
-- | In-place   | No         |
-- +------------+------------+
-- | Stable     | Yes        |
-- +------------+------------+
--
-- == Complexity
--
-- This runs in \(O(1)\) time, performing exactly three
-- compare-and-swap operations.
sort3 :: Ord a => a -> a -> a -> (a, a, a)
{-# INLINEABLE sort3 #-}
sort3 x0 y0 z0 =
  let cswap x y
        | x <= y = (x, y)
        | otherwise = (y, x)

      ((x1, y1), z1) = (cswap x0 y0, z0) -- x1 <= y1
      (x2, (y2, z2)) = (x1, cswap y1 z1) -- y2 <= z2, x2 <= z2
      ((x3, y3), z3) = (cswap x2 y2, z2) -- x3 <= y3 <= z3
   in (x3, y3, z3)
