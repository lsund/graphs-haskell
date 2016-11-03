
module DGraph where

import Data.Maybe
import Prelude as P
import Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Types 
import EdgeSet
import VertexSet
import Graph
import AdjacencyList

data DGraph = DGraph AdjacencyList deriving (Show)

instance Graph DGraph where
    empty                   = DGraph M.empty
    singleton v             = DGraph $ M.singleton v S.empty
    vertices (DGraph m)     = S.union (S.unions $ M.elems m) (S.fromList $ M.keys m)
    edges (DGraph m)        = DEdgeSet $ AdjacencyList.edges m
    null graph              = EdgeSet.null $ Graph.edges graph
    neighbours v (DGraph m) = fromMaybe S.empty (M.lookup v m)

dGraph :: DEdgeSet -> DGraph
dGraph es = DGraph (adjacencyList es M.empty)

induced ::  (DEdgeSet -> DGraph) -> VertexSet -> DGraph -> DGraph
induced f vs m            =
    let inducedEdges = S.filter (VertexSet.edges vs) (EdgeSet.edges (Graph.edges m))
     in f (DEdgeSet inducedEdges)

