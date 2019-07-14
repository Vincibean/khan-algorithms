module Algo.MergeSort
    ( mergeSort
    )
where

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [a] = [a]
mergeSort as  = merge mergedFirstHalf mergedSecondHalf
  where
    mergedFirstHalf  = mergeSort firstHalf
    mergedSecondHalf = mergeSort secondHalf
    firstHalf        = take half as
    secondHalf       = drop half as
    half             = length as `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge as [] = as
merge [] as = as
merge f@(fh : ft) s@(sh : st) | fh < sh   = fh : merge ft s
                              | otherwise = sh : merge f st
