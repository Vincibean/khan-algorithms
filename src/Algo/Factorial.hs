module Algo.Factorial
    ( iterFactorial
    )
where

iterFactorial :: Enum a => Num a => a -> a
iterFactorial n = product [1 .. n]
