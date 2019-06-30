module Algo.InsertionSortSpec
  ( spec
  )
where

import           Test.Hspec
import           Algo.InsertionSort             ( insertionSort
                                                , insert'
                                                )

spec :: Spec
spec = do
  insertionSortSpec
  insertSpec'

insertionSortSpec :: Spec
insertionSortSpec = describe "insertionSort" $ do
  it "can sort an empty list" $ do
    let array = [] :: [Int]
    insertionSort array `shouldBe` []
  it "can sort a list with one element" $ insertionSort [23] `shouldBe` [23]
  it "works with ordered lists" $ insertionSort [2, 3] `shouldBe` [2, 3]
  it "sorts elements in a list" $ insertionSort [1, 3, 2] `shouldBe` [1, 2, 3]

insertSpec' :: Spec
insertSpec' = describe "insert'" $ do
  it "prepends elements to a list" $ insert' 0 1 [2, 3] `shouldBe` [1, 2, 3]
  it "appends elements to a list" $ insert' 2 1 [2, 3] `shouldBe` [2, 3, 1]
  it "prepends elements to a list even if the index is unreasonably low"
    $          insert' (-10) 1 [2, 3]
    `shouldBe` [1, 2, 3]
  it "appends elements to a list even if the index is unreasonably high"
    $          insert' 20 1 [2, 3]
    `shouldBe` [2, 3, 1]
