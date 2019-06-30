module Algo.InsertionSort
  ( insertionSort
  , insert'
  )
where

import           Data.List                      ( findIndex )
import           Data.Maybe                     ( fromMaybe )

insertionSort :: Ord a => [a] -> [a]
insertionSort []      = []
insertionSort [x    ] = [x]
insertionSort (h : t) = insertRec [h] t

insertRec :: Ord a => [a] -> [a] -> [a]
insertRec as []        = as
insertRec fh (el : sh) = insertRec ofh sh
 where
  ofh           = insert' rightPosition el fh
  rightPosition = fromMaybe lastPosition maybeIndex
  lastPosition  = length fh
  maybeIndex    = findIndex (>= el) fh

insert' :: Int -> a -> [a] -> [a]
insert' pos el as = let (xs, ys) = splitAt pos as in xs ++ [el] ++ ys
