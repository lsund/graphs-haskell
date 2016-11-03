
module Bipartite where

import Data.List
import qualified Data.Set as S
import qualified Data.Dequeue as Q

import Types 
import Graph
import UGraph
import DGraph
import Tree 
import VertexSet 
import GraphScanning as GS
import EdgeSet

type UBiGraph = (UGraph, (VertexSet, VertexSet))
type DBiGraph = (DGraph, (VertexSet, VertexSet))
--
-- Dummy Data ---------------------------------------------------------------

bpg = uGraph (DEdgeSet $ S.fromList [(0, 3), (1, 3), (1, 4), (1, 5), (2, 5)])

dbpg = dGraph (DEdgeSet $ S.fromList [(0, 3), (1, 3), (1, 4), (1, 5), (2, 5)])

nbpg = uGraph (DEdgeSet $ S.fromList [(0, 3), (0, 1), (1, 3), (1, 4), (2, 4), (2, 5)])

sampleMatching = DEdgeSet $ S.fromList [(0, 3), (1, 4), (2, 5)]

sampleQueue :: Queue Edge
sampleQueue = Q.fromList [(0,1), (0,2), (1, 3), (1, 4), (3, 5)]

sampleTree = fromEdgeQueue sampleQueue

sampleEdges = DEdgeSet $ S.fromList [(0, 1), (1, 2), (0, 2)]
ugraph = uGraph sampleEdges

----------------------------------------------------------------------------

everyf :: Int -> [a] -> [a]
everyf n [] = []
everyf n as  = head as : everyf n (drop n as)
every n = everyf n . drop (n-1)

part :: UGraph -> UBiGraph
part graph =
    let 
        (_, q) = GS.bfs graph 0
        ls = levels $ fromEdgeQueue q
        (evens, odds) = (every 2 ls, everyf 2 ls)
        (as, bs)      = (S.fromList (concat evens), S.fromList (concat odds))
     in (graph, (as, bs))

{-isBipartite :: UGraph -> Bool-}
{-isBipartite graph = Graph.null (UGraph.induced as graph) && Graph.null (UGraph.induced bs graph) -}
    {-where (_, (as, bs)) = part graph-}

{-undirToDir :: UBiGraph -> DBiGraph-}
{-undirToDir graph = edges-}

{-existsMAugmentingPath :: UBiGraph -> Bool -}
existsMAugmentingPath matching graph = 
    if validMatching matching 
       then Just $ existsMAugmentingPath' matching graph
       else Nothing

existsMAugmentingPath' matching graph =
    let s = succ (S.findMax vs)
        t = succ s
        vs' = vs `S.union` S.singleton s `S.union` S.singleton t 
     in vs'
    where vs = Graph.vertices graph


