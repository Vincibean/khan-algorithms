module Algo.BinarySearch
  ( binarySearch
  )
where

binarySearch :: (Num a, Integral a) => a -> [a] -> Maybe Int
binarySearch needle haystack =
  binarySearchRec needle haystack 0 (length haystack)

binarySearchRec :: (Num a, Integral a) => a -> [a] -> Int -> Int -> Maybe Int
binarySearchRec needle haystack lo hi
  | hi < lo        = Nothing
  | pivot > needle = binarySearchRec needle haystack lo (mid - 1)
  | pivot < needle = binarySearchRec needle haystack (mid + 1) hi
  | otherwise      = Just mid
 where
  mid   = lo + (hi - lo) `div` 2
  pivot = haystack !! mid
