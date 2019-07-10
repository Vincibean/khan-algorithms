module Algo.IsPalindrome
    ( isPalindrome
    )
where

isPalindrome :: String -> Bool
isPalindrome []        = True
isPalindrome [  _    ] = True
isPalindrome s@(h : t) = last s == h && isPalindrome (init t)
