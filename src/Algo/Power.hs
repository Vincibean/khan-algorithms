module Algo.Power
    ( power
    )
where

power :: (Fractional a, Integral b) => a -> b -> a
power base exp
    | exp < 0
    = recip $ power base $ negate exp
    | exp > 0 && odd exp
    = base * power base (exp - 1)
    | exp > 0 && even exp
    = let res = power base $ round $ fromIntegral exp / 2 in res * res
    | otherwise
    = 1
