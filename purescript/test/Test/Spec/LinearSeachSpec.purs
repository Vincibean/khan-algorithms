module Test.Spec.LinearSearchSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import LinearSearch (doLinearSearch, doLinearSearch', doLinearSearch'')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  doLinearSearchSpec
  doLinearSearchSpec'
  doLinearSearchSpec''
  
doLinearSearchSpec :: Spec Unit
doLinearSearchSpec = 
  describe "doLinearSearch" do
    it "returns -1 if it couldn't find a value in an array" do
      doLinearSearch  42 [1, 2, 3] `shouldEqual` -1
    it "return the index of the value if present (first position)" do
      doLinearSearch 1 [1, 2, 3] `shouldEqual` 0
    it "return the index of the value if present (last position)" do
      doLinearSearch 3 [1, 2, 3] `shouldEqual` 2
 
doLinearSearchSpec' :: Spec Unit
doLinearSearchSpec' = 
  describe "doLinearSearch'" do
    it "returns Nothing if it couldn't find a value in an array" do
      doLinearSearch'  42 [1, 2, 3] `shouldEqual` Nothing
    it "return the index of the value if present (first position)" do
      doLinearSearch' 1 [1, 2, 3] `shouldEqual` Just 0
    it "return the index of the value if present (last position)" do
      doLinearSearch' 3 [1, 2, 3] `shouldEqual` Just 2

doLinearSearchSpec'' :: Spec Unit
doLinearSearchSpec'' = 
  describe "doLinearSearch''" do
    it "returns Nothing if it couldn't find a value in an array" do
      doLinearSearch''  42 [1, 2, 3] `shouldEqual` Nothing
    it "return the index of the value if present (first position)" do
      doLinearSearch'' 1 [1, 2, 3] `shouldEqual` Just 0
    it "return the index of the value if present (last position)" do
      doLinearSearch'' 3 [1, 2, 3] `shouldEqual` Just 2