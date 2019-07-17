module Algo.QuickSort
    ( quickSort
    )
where

quickSort :: Ord a => [a] -> [a]
quickSort []      = []
quickSort (h : t) = quickSort lesserThan ++ [h] ++ quickSort greaterThan
  where
    lesserThan  = [ x | x <- t, x <= h ]
    greaterThan = [ x | x <- t, x > h ]
