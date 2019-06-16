module Algo.IndexOfMinimum
    ( indexOfMinimum
    )
where

indexOfMinimum :: Ord a => Int -> [a] -> Maybe Int
indexOfMinimum i as = if validIndex then Just minIndex else Nothing
  where
    validIndex = i >= 0 && i < length as
    minIndex   = indexOfMinimum' curMin zips
    zips       = zipWithIndex as
    curMin     = zips !! i

indexOfMinimum' :: Ord a => (a, Int) -> [(a, Int)] -> Int
indexOfMinimum' (curMin, curMinPos) [] = curMinPos
indexOfMinimum' x@(curMin, curMinPos) (y@(a, i) : xs) =
    if a < curMin then indexOfMinimum' y xs else indexOfMinimum' x xs

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex as = zip as [0 ..]
