module Algo.SelectionSort
    ( selectionSort
    )
where

import           Algo.Swap                      ( swap )
import           Algo.IndexOfMinimum            ( indexOfMinimum )
import           Data.Maybe                     ( fromMaybe )

selectionSort :: Ord a => [a] -> [a]
selectionSort []         = []
selectionSort as@(h : t) = swappedHead : selectionSort swappedTail
  where
    (swappedHead : swappedTail) = swapped
    swapped                     = swap 0 minimumIndex as
    minimumIndex                = fromMaybe 0 maybeMinimumIndex
    maybeMinimumIndex           = indexOfMinimum 0 as
