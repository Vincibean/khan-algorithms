module Algo.QuickSortSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.QuickSort                 ( quickSort )

spec :: Spec
spec = describe "quickSort" $ do
    it "returns the empty list if the empty list is given" $ do
        let is = [] :: [Int]
        quickSort is `shouldBe` is
    it
            "returns a list with only one element if a list with only one element is given"
        $          quickSort [1]
        `shouldBe` [1]
    it
            "returns a sorted list with two elements if an unsorted list with two elements is given"
        $          quickSort [2, 1]
        `shouldBe` [1, 2]
    it "returns a sorted list in all the other cases"
        $          quickSort [9, 4, 7, 0, 27, 2, 1]
        `shouldBe` [0, 1, 2, 4, 7, 9, 27]

