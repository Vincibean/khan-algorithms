module Algo.LinearSearchSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.LinearSearch              ( linearSearch )


spec :: Spec
spec = describe "linearSearch" $ do
    it "returns Nothing if it couldn't find a value in an array"
        $          linearSearch 42 [1, 2, 3]
        `shouldBe` Nothing
    it "returns the index of the value if present (first position)"
        $          linearSearch 1 [1, 2, 3]
        `shouldBe` Just 0
    it "returns the index of the value if present (last position)"
        $          linearSearch 3 [1, 2, 3]
        `shouldBe` Just 2
    it "returns the index of the value if present (middle position)"
        $          linearSearch 2 [1, 2, 3]
        `shouldBe` Just 1
