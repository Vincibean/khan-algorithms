module BinarySearch 
       ( doUnsafeSearch
       ) where

import Prelude

import Data.Array (length, slice, unsafeIndex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

doUnsafeSearch :: forall a. Eq a => Ord a => a -> Array a -> Int
doUnsafeSearch = doUnsafeSearchRec 0
          

split :: forall a. Array a -> Tuple (Array a) (Array a)
split [] = Tuple [] []
split as = Tuple fh sh
  where fh = slice 0 m as
        sh = slice m l as
        m = l / 2
        l = length as


doUnsafeSearchRec :: forall a. Eq a => Ord a => Int -> a -> Array a -> Int
doUnsafeSearchRec _ _ [] = -1
doUnsafeSearchRec tpos targetValue as = 
                          if (mid == targetValue)
                          then resultPos
                          else if (mid < targetValue) 
                          then doUnsafeSearchRec updatedPos targetValue sh
                          else doUnsafeSearchRec tpos targetValue fh
                            where mid = unsafePartial $ unsafeIndex as guess
                                  guess = (max + min) / 2
                                  min = 0
                                  max = length as - 1
                                  (Tuple fh sh) = split as
                                  resultPos = tpos + guess
                                  updatedPos = tpos + length fh