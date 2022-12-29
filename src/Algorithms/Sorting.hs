module Algorithms.Sorting
  ( -- * General-purpose

    -- | These sorting algorithms are suitable for most use cases.
    quickSort,
    mutQuicksort,

    -- * For small lists

    -- | These sorting algorithms have poor worst-case complexity, and
    -- so are useful on small lists (or possibly nearly-sorted lists.)
    insertionSort,
    mutInsertionSort,

    -- * For integers

    -- | Linear-time sorting algorithms, specialized for sorting integers.
    countingSort,

    -- * Specialized sorts
    sort3,
  )
where

import Algorithms.Sorting.CountingSort
import Algorithms.Sorting.InsertionSort
import Algorithms.Sorting.QuickSort
import Algorithms.Sorting.Sort3
