module Algo.IndexOfMinimumSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.IndexOfMinimum            ( indexOfMinimum )


spec :: Spec
spec = describe "indexOfMinimum" $ do
    it "returns Nothing if the input index is negative"
        $          indexOfMinimum (-1) [1, 2, 3]
        `shouldBe` Nothing
    it "returns Nothing if the input index is too large"
        $          indexOfMinimum 100 [1, 2, 3]
        `shouldBe` Nothing
    it "returns Nothing if the list is empty" $ do
        let em = [] :: [Int]
        indexOfMinimum 0 em `shouldBe` Nothing
    it "returns 0 if the index of the minimum is in first position"
        $          indexOfMinimum 0 [1, 2, 3]
        `shouldBe` Just 0
    it
            "returns 1 if the index of the minimum is in second position and the input index is 1"
        $          indexOfMinimum 1 [4, 1, 2, 3]
        `shouldBe` Just 1
    it
            "returns n (the maximum index) if the index of the minimum is in last position"
        $          indexOfMinimum 1 [4, 1, 2, 3, 0]
        `shouldBe` Just 4
    it
            "returns n (a middle index) if the index is somewhere in the middle of the list"
        $          indexOfMinimum 1 [4, 1, 0, 3, 4]
        `shouldBe` Just 2
    it
            "returns n (the first valid index) if the list contains multiple min values"
        $          indexOfMinimum 1 [4, 1, 1, 1, 1]
        `shouldBe` Just 1
