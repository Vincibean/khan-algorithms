module Test.Spec.BinarySearchSpec (spec) where

import Prelude

import BinarySearch (doUnsafeSearch)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  doUnsafeSearchSpec
  
--- Program.assertEqual(doSearch(primes, 73), 20);
--- Program.assertEqual(doSearch(primes, 72), -1);
--- Program.assertEqual(doSearch(primes, 97), 24);

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

doUnsafeSearchSpec :: Spec Unit
doUnsafeSearchSpec = 
  describe "doUnsafeSearch" do
    it "return the index of the value if present (first position, 1 elements array)" do
      doUnsafeSearch 1 [1] `shouldEqual` 0
    it "return the index of the value if present (second position, 2 elements array)" do
      doUnsafeSearch 1 [0, 1] `shouldEqual` 1
    it "return the index of the value if present (middle position)" do
      doUnsafeSearch 73 primes `shouldEqual` 20
    it "returns -1 if it couldn't find a value in an array" do
      doUnsafeSearch 72 primes `shouldEqual` -1
    it "return the index of the value if present (last position)" do
      doUnsafeSearch 97 primes `shouldEqual` 24