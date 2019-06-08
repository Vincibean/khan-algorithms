module Algo.LinearSearch
    ( linearSearch
    )
where

import           Data.List                      ( findIndex )

linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch el = findIndex (== el)
