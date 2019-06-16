module Algo.SwapSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.Swap                      ( swap )


spec :: Spec
spec = describe "swap" $ do
    it "returns the same list if the first index isn't valid"
        $          swap 1000 0 [1]
        `shouldBe` [1]
    it "returns the same list if the second index isn't valid"
        $          swap 0 1000 [1]
        `shouldBe` [1]
    it "returns the same list if both indices aren't valid"
        $          swap 1000 1000 [1]
        `shouldBe` [1]
    it "returns the same list if both indices are the same"
        $          swap 1 1 [0, 1]
        `shouldBe` [0, 1]
    it "returns the same list if the first index isn't positive"
        $          swap (-1) 0 [1]
        `shouldBe` [1]
    it "returns the same list if the second index isn't positive"
        $          swap 0 (-1) [1]
        `shouldBe` [1]
    it "returns the same list if both indices aren't positive"
        $          swap (-1) (-1) [1]
        `shouldBe` [1]
    it "returns the same list if both indices are the same"
        $          swap 1 1 [0, 1]
        `shouldBe` [0, 1]
    it "returns a list with elements swapped at the given indices"
        $          swap 0 1 [0, 1]
        `shouldBe` [1, 0]
