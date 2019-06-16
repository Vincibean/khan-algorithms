module Algo.Swap
  ( swap
  )
where

swap :: Int -> Int -> [a] -> [a]
swap firstIndex secondIndex as = if pred then as' else as
 where
  pred =
    firstIndex
      >= 0
      && firstIndex
      <  l
      && secondIndex
      >= 0
      && secondIndex
      <  l
      && firstIndex
      /= secondIndex
  l   = length as
  f   = as !! firstIndex
  s   = as !! secondIndex
  as' = putAt firstIndex s $ putAt secondIndex f as


putAt :: Int -> a -> [a] -> [a]
putAt pos el as = let (xs, ys) = splitAt pos as in xs ++ [el] ++ tail ys
