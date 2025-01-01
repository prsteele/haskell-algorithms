module Algorithms.Sorting
  ( -- * General-purpose sorts

    -- | These sorting algorithms are suitable for most use cases.

    -- ** Quicksort
    quicksort,
    quicksortBy,
    quicksortOn,
    mutQuicksort,
    mutQuicksortBy,
    mutQuicksortOn,

    -- ** Merge sort
    mergeSort,
    mergeSortBy,
    mergeSortOn,
    mutMergeSort,
    mutMergeSortBy,
    mutMergeSortOn,

    -- ** Heap sort
    heapsort,
    heapsortBy,
    heapsortOn,
    mutHeapsort,
    mutHeapsortBy,
    mutHeapsortOn,

    -- * Sorts for small lists

    -- | These sorting algorithms have poor worst-case complexity, and
    -- so are useful on small lists (or possibly nearly-sorted lists.)
    insertionSort,
    insertionSortBy,
    insertionSortOn,
    mutInsertionSort,
    mutInsertionSortBy,
    mutInsertionSortOn,

    -- * Linear-time sorts

    -- ** Counting sort
    countingSort,

    -- ** Radix sort
    radixSort,
    mutRadixSort,
    twosComplementRadixSort,
    mutTwosComplementRadixSort,

    -- * Constant-time sorts
    sort3,
    sort3By,
    sort3On,
  )
where

import Algorithms.Sorting.CountingSort
import Algorithms.Sorting.HeapSort
import Algorithms.Sorting.InsertionSort
import Algorithms.Sorting.MergeSort
import Algorithms.Sorting.Quicksort
import Algorithms.Sorting.RadixSort
import Algorithms.Sorting.Sort3
