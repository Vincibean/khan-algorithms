module Algo.MergeSortSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.MergeSort                 ( mergeSort )

spec :: Spec
spec = describe "mergeSort" $ do
    it "returns an empty list if an empty list is given" $ do
        let em = [] :: [Int]
        mergeSort em `shouldBe` em
    it "returns a single element list if a single element list is given"
        $          mergeSort [42]
        `shouldBe` [42]
    it "returns a sorted list (even number of elements)"
        $          mergeSort [4, 3, 1, 2]
        `shouldBe` [1, 2, 3, 4]
    it "returns a sorted list (odd number of elements)"
        $          mergeSort [4, 3, 1]
        `shouldBe` [1, 3, 4]
    it "returns a sorted list (with 0)"
        $          mergeSort [4, 3, 0, 1]
        `shouldBe` [0, 1, 3, 4]
    it "returns a sorted list (with negative numbers)"
        $          mergeSort [4, 3, 0, -1]
        `shouldBe` [-1, 0, 3, 4]
