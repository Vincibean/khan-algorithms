module Test.Spec.DiscoverySpec where

import Prelude

import LinearSearch (doLinearSearch)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "doLinearSearch" do
    it "returns -1 if it couldn't find a value in an array" do
      doLinearSearch  42 [1, 2, 3] `shouldEqual` -1