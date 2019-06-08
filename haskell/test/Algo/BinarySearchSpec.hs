module Algo.BinarySearchSpec
    ( spec
    )
where

import           Test.Hspec

import           Algo.BinarySearch              ( binarySearch )

primes :: [Int]
primes =
    [ 2
    , 3
    , 5
    , 7
    , 11
    , 13
    , 17
    , 19
    , 23
    , 29
    , 31
    , 37
    , 41
    , 43
    , 47
    , 53
    , 59
    , 61
    , 67
    , 71
    , 73
    , 79
    , 83
    , 89
    , 97
    ]

spec :: Spec
spec = do
    describe "doUnsafeSearch" $ do
        it
                "returns the index of the value if present (first position, 1 elements array)"
            $ do
                  binarySearch [1] 1 `shouldBe` Just 0
        it
                "returns the index of the value if present (second position, 2 elements array)"
            $ do
                  binarySearch [0, 1] 1 `shouldBe` Just 1
        it "returns the index of the value if present (middle position)" $ do
            binarySearch primes 73 `shouldBe` Just 20
        it "returns Nothing if it couldn't find a value in an array" $ do
            binarySearch primes 72 `shouldBe` Nothing
        it "returns the index of the value if present (last position)" $ do
            binarySearch primes 97 `shouldBe` Just 24
