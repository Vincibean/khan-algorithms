module Algo.IsPalindromeSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.IsPalindrome              ( isPalindrome )

spec :: Spec
spec = describe "isPalindrome" $ do
    it "returns True if given an empty String" $ isPalindrome "" `shouldBe` True
    it "returns True if given a 1 Char String"
        $          isPalindrome "a"
        `shouldBe` True
    it "returns False if the first and last Chars are different"
        $          isPalindrome "motor"
        `shouldBe` False
    it "returns False if the first and last Chars are different"
        $          isPalindrome "motor"
        `shouldBe` False
    it "returns True if the String is a palindrome"
        $          isPalindrome "rotor"
        `shouldBe` True
    it "returns True if the String contains spaces only"
        $          isPalindrome "   "
        `shouldBe` True
