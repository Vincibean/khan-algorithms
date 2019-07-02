module Algo.FactorialSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.Factorial                 ( iterFactorial
                                                , recFactorial
                                                )


spec :: Spec
spec = do
    iterFactorialSpec
    recFactorialSpec

iterFactorialSpec :: Spec
iterFactorialSpec = describe "iterFactorial" $ do
    it "returns 1 if 0 is given" $ iterFactorial 0 `shouldBe` 1
    it "returns 1 if 1 is given" $ iterFactorial 1 `shouldBe` 1
    it "returns a factorial if a positive integer is given"
        $          iterFactorial 5
        `shouldBe` (5 * 4 * 3 * 2 * 1)


recFactorialSpec :: Spec
recFactorialSpec = describe "recFactorial" $ do
    it "returns 1 if 0 is given" $ recFactorial 0 `shouldBe` 1
    it "returns 1 if 1 is given" $ recFactorial 1 `shouldBe` 1
    it "returns a factorial if a positive integer is given"
        $          recFactorial 5
        `shouldBe` (5 * 4 * 3 * 2 * 1)
