module LinearSearch where

import Data.Array (elemIndex, findIndex, length, range, uncons, zipWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Eq, negate, (-), (==))

-- var doLinearSearch = function(array, targetValue) {
--   for (var guess = 0; guess < array.length; guess++) {
--     if (array[guess] === targetValue) { 
--         return guess;  // found it!
--     }
--   }
--   return -1;  // didn't find it
-- };

doLinearSearch :: forall a. Eq a => a -> Array a -> Int
doLinearSearch t xs = innerDoLinearSearch t as
  where as = zipWithIndex xs
        innerDoLinearSearch t as = case uncons as of
          Nothing -> -1
          Just { head: h, tail: ts } | snd h == t -> fst h
          Just { head, tail: ts } -> innerDoLinearSearch t ts
        
doLinearSearch' :: forall a. Eq a => a -> Array a -> Maybe Int
doLinearSearch' x = findIndex (_ == x)

doLinearSearch'' :: forall a. Eq a => a -> Array a -> Maybe Int
doLinearSearch'' = elemIndex

zipWithIndex :: forall a. Array a -> Array (Tuple Int a)
zipWithIndex xs = zipWith (\x y -> Tuple x y) (range 0 (length xs - 1)) xs