module Algo.BreadthFirstSearchSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.BreadthFirstSearch

g :: Graph Int
g = Graph
    [ Vertex 1 [2, 3]
    , Vertex 2 [1, 4, 5]
    , Vertex 3 [1, 4]
    , Vertex 4 [2, 3, 5]
    , Vertex 5 [2, 4, 6, 7]
    , Vertex 6 [5, 7, 8]
    , Vertex 7 [5, 6, 9]
    , Vertex 8 [6, 9]
    , Vertex 9 [7, 8]
    ]

spec :: Spec
spec = describe "doBFS" $ it "works" $ do
    let res = verticesDescriptions (doBFS g (head (vertices g)))
    _ <- head res `shouldBe` VertexDescr { distance = 0, predecessor = Nothing }
    head (tail res)
        `shouldBe` VertexDescr { distance = 1, predecessor = Just 1 }
