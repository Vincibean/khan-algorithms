-----------------------------------------------------------------------------
--
-- Inspired by
--
-- https://lettier.github.io/posts/2016-04-29-breadth-first-search-in-haskell.html
--
-----------------------------------------------------------------------------

module Algo.BreadthFirstSearch
  ( doBFS
  , Vertex(..)
  , Graph(..)
  , VertexDescr(..)
  , GraphDescr(..)
  )
where

data Vertex a = Vertex {
                          vertexLabel :: a
                        , vertexNeighbors :: [a]
                      } deriving (Show, Eq)

newtype Graph a = Graph { vertices :: [Vertex a] } deriving (Show, Eq)

data VertexDescr a = VertexDescr {
                                    distance :: Int
                                  , predecessor :: Maybe a
                                } deriving (Show, Eq)

newtype GraphDescr a = GraphDescr { verticesDescriptions :: [VertexDescr a] }  deriving (Show, Eq)

doBFS :: (Num a, Eq a) => Graph a -> Vertex a -> GraphDescr a
doBFS inGraph source = doBFS' inGraph outGraph queue seen 0
 where
  queue    = [source]
  outGraph = GraphDescr [VertexDescr 0 Nothing]
  seen     = queue

doBFS'
  :: (Num a, Eq a)
  => Graph a       -- In
  -> GraphDescr a  -- Out
  -> [Vertex a]    -- Queue
  -> [Vertex a]    -- Seen
  -> Int           -- Distance of the previous node
  -> GraphDescr a  -- Out
doBFS' (Graph [])      _                    _       _    _    = GraphDescr []
doBFS' _               outGraph             []      _    _    = outGraph
doBFS' (Graph (a : b)) (GraphDescr (c : d)) (e : f) seen dist = doBFS'
  inGraph
  outGraph
  queue
  seen'
  (newDist + 1)
 where
  inGraph           = Graph (a : b)
  currLabel         = vertexLabel e
  eNeighbors        = vertexNeighbors e
  eVertexNeighbors  = verticesByLabels inGraph eNeighbors
  newDist           = dist + 1
  filteredNeighbors = filterVertexNeighbors seen eVertexNeighbors -- Remove all neighbors that have been queued up before.
  enqueueDescr      = replicate
    (length filteredNeighbors)
    VertexDescr { distance = newDist, predecessor = Just currLabel }
  outGraph = GraphDescr $ (c : d) ++ enqueueDescr
  queue    = f ++ filteredNeighbors
  seen'    = seen ++ filteredNeighbors

verticesByLabels :: Eq a => Graph a -> [a] -> [Vertex a]
verticesByLabels (Graph []     ) _  = []
verticesByLabels (Graph (x : y)) [] = x : y
verticesByLabels (Graph (x : y)) keys =
  filter (\z -> vertexLabel z `elem` keys) (x : y)

filterVertexNeighbors :: Eq a => [Vertex a] -> [Vertex a] -> [Vertex a]
filterVertexNeighbors _  [] = []
filterVertexNeighbors [] _  = []
filterVertexNeighbors s  vn = filter (not . vertexInVertexes s) vn

vertexInVertexes :: Eq a => [Vertex a] -> Vertex a -> Bool
vertexInVertexes [] _ = False
vertexInVertexes vs v = vertexLabel v `elem` map vertexLabel vs
