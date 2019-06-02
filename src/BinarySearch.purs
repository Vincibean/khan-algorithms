module BinarySearch (doUnsafeSearch) where

import Prelude

import Data.Array (length, unsafeIndex, slice)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

--- /* Returns either the index of the location in the array,
---   or -1 if the array did not contain the targetValue */
--- var doSearch = function(array, targetValue) {
--- 	var min = 0;
--- 	var max = array.length - 1;
---     var guess;
---     var guesses = 0;
---     while(max >= min){
---       guess = Math.floor((max + min) / 2);
---       guesses = guesses + 1;
---       println(guess);
---       if(array[guess] === targetValue){ 
---           println(guesses); 
---           return guess; 
---       }
---       else if (array[guess] < targetValue){ min = guess + 1; }
---       else{ max = guess - 1; }
---     }
--- 	return -1; 
--- };
--- 
--- var primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 
--- 		41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97];
--- 
--- var result = doSearch(primes, 97);
--- println("Found prime at index " + result);
--- 
--- Program.assertEqual(doSearch(primes, 73), 20);
--- Program.assertEqual(doSearch(primes, 72), -1);
--- Program.assertEqual(doSearch(primes, 97), 24);

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
doUnsafeSearchRec tpos targetValue [] = -1
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