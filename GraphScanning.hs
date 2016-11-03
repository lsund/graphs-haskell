
module GraphScanning where

import Data.Maybe
import qualified Data.Set as S
import qualified Data.Dequeue as Q

import Graph
import VertexSet
import UGraph
import Types

-- DepthFirstSearch/BreadthFirstSearch
dfs :: UGraph -> Vertex -> (VertexSet, Queue Edge)
dfs = graphScanningAlgorithm Q.popFront

bfs :: UGraph -> Vertex -> (VertexSet, Queue Edge)
bfs = graphScanningAlgorithm Q.popBack

-- GRAPH SCANNING ALGORITHM
-- Input: 
-- A undirected graph G 
-- A source vertex s
-- A way to choose an item from a queue, typically FIFO (DFS) or LIFO (BFS)
-- Output: 
-- The vertex set R reachable from s
-- A set T subset of E(G) such that (R,T) is a tree rooted at s  
graphScanningAlgorithm
    :: (Queue Vertex -> Maybe (Vertex, Queue Vertex))
    -> UGraph
    -> Vertex
    -> (VertexSet, Queue Edge)
graphScanningAlgorithm choose graph s = 
    let 
        visited = S.fromList [s]
        queue   = Q.fromList [s]
        tree    = Q.empty
    in 
        graphScanningAlgorithm2 choose graph queue visited tree

graphScanningAlgorithm2 
    :: (Queue Vertex -> Maybe (Vertex, Queue Vertex))
    -> UGraph 
    -> Queue Vertex 
    -> VertexSet 
    -> Queue Edge
    -> (VertexSet, Queue Edge)
graphScanningAlgorithm2 choose graph queue visited tree = 
    case choose queue of 
        Nothing     -> (visited, tree)
        Just (v, _) -> graphScanningAlgorithm3 choose graph queue visited tree v

graphScanningAlgorithm3 
    :: (Queue Vertex -> Maybe (Vertex, Queue Vertex))
    -> UGraph 
    -> Queue Vertex
    -> VertexSet 
    -> Queue Edge
    -> Vertex     
    -> (VertexSet, Queue Edge)
graphScanningAlgorithm3 choose graph queue visited tree v = 
    if S.null unbs
        then 
            let (_, popped) = fromJust $ choose queue
            in graphScanningAlgorithm2 choose graph popped visited tree
        else 
            let 
                w = S.elemAt 0 unbs
            in 
                graphScanningAlgorithm2 
                    choose
                    graph
                    (Q.pushFront queue w)
                    (S.insert w visited)
                    (Q.pushBack tree (v, w))
        where unbs = unvisitedNeighbours v visited graph 

unvisitedNeighbours 
    :: Vertex
    -> VertexSet
    -> UGraph 
    -> VertexSet
unvisitedNeighbours v visited graph = unvisited `S.intersection` nbs
    where 
        nbs = neighbours v graph
        unvisited  = (S.\\) (vertices graph) visited


