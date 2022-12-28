module Algorithms.Sequence where

downTo :: Integral a => a -> a -> [a]
i `downTo` j = [i, i - 1 .. j]

upTo :: Integral a => a -> a -> [a]
i `upTo` j = [i .. j]
