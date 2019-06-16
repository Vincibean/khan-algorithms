module Algo.SelectionSortSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.SelectionSort             ( selectionSort )


spec :: Spec
spec = describe "selectionSort" $ do
    it "returns an empty list if an empty list is given" $ do
        let em = [] :: [Int]
        selectionSort em `shouldBe` em
    it "returns a sorted list" $ selectionSort [2, 3, 1] `shouldBe` [1, 2, 3]
