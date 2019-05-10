module Main where

import Prelude

import BinarySearch (doSearch)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ doSearch [1, 2, 3] 2
