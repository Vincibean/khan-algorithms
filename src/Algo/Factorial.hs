module Algo.Factorial
    ( iterFactorial
    , recFactorial
    )
where

iterFactorial :: (Enum a, Num a) => a -> a
iterFactorial n = product [1 .. n]

recFactorial :: (Eq a, Num a) => a -> a
recFactorial 0 = 1
recFactorial n = n * recFactorial (n - 1)
