module Algo.PowerSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.Power                     ( power )

spec :: Spec
spec = describe "power" $ do
    it "returns 1 if the exponent is 0" $ power 3 0 `shouldBe` 1
    it "returns the same number if the exponent is 1" $ power 3 1 `shouldBe` 3
    it "returns the power if the exponent is greater than 1 (even case)"
        $          power 3 2
        `shouldBe` 9
    it "returns the power if the exponent is greater than 1 (odd case)"
        $          power 3 3
        `shouldBe` 27
    it "returns the power (using the reciprocal) if the exponent is negative"
        $          power 3 (negate 1)
        `shouldBe` (fromInteger 1 / 3)

