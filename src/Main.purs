module Main where

import Prelude

import BinarySearch (doUnsafeSearch)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ doUnsafeSearch [1, 2, 3] 2
