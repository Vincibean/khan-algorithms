module Algo.BinarySearch
  ( binarySearch
  )
where

import           Data.Array

binarySearch :: (Num a, Integral a, Ix a) => Array Int a -> a -> Maybe Int
binarySearch haystack needle =
  binarySearchRec haystack needle 0 (length haystack)

binarySearchRec
  :: (Num a, Integral a, Ix a) => Array Int a -> a -> Int -> Int -> Maybe Int
binarySearchRec haystack needle lo hi
  | hi < lo        = Nothing
  | pivot > needle = binarySearchRec haystack needle lo (mid - 1)
  | pivot < needle = binarySearchRec haystack needle (mid + 1) hi
  | otherwise      = Just mid
 where
  mid   = lo + (hi - lo) `div` 2
  pivot = haystack ! mid
