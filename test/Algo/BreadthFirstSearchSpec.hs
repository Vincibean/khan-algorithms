module Algo.BreadthFirstSearchSpec
    ( spec
    )
where

import           Test.Hspec
import           Algo.BreadthFirstSearch

fullGraph :: Graph Int
fullGraph = Graph
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

emptyGraph :: Graph Int
emptyGraph = Graph []

source :: Vertex Int
source = head . vertices $ fullGraph

spec :: Spec
spec = describe "doBFS" $ do
    it "return an empty description if an empty graph is given"
        $          verticesDescriptions (doBFS emptyGraph source)
        `shouldBe` []
    it "return the expected result in the other cases" $ do
        let res = verticesDescriptions (doBFS fullGraph source)
        _ <- head res `shouldBe` expFst
        second res `shouldBe` expSnd
  where
    second = head . tail
    expFst = VertexDescr { distance = 0, predecessor = Nothing }
    expSnd = VertexDescr { distance = 1, predecessor = Just 1 }
