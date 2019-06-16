module Algo.LinearSearch
    ( linearSearch
    )
where

import           Data.List                      ( elemIndex )

linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch = elemIndex
