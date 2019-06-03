module Lib
    ( binarySearch
    )
where

import           Data.Array

binarySearch haystack needle lo hi
    | hi < lo        = Nothing
    | pivot > needle = binarySearch haystack needle lo (mid - 1)
    | pivot < needle = binarySearch haystack needle (mid + 1) hi
    | otherwise      = Just mid
  where
    mid   = lo + (hi - lo) `div` 2
    pivot = haystack ! mid
