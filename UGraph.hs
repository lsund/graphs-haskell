
module UGraph where

import Data.Maybe
import Data.Tuple
import Prelude as P 
import Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import EdgeSet 
import VertexSet
import Types
import Graph
import AdjacencyList
import DGraph

data UGraph = UGraph AdjacencyList deriving (Show)

instance Graph UGraph where
    empty                   = UGraph M.empty
    singleton v             = UGraph $ M.singleton v S.empty
    vertices (UGraph m)     = S.union (S.unions $ M.elems m) (S.fromList $ M.keys m)
    edges (UGraph m)        = UEdgeSet $ AdjacencyList.edges m
    null graph              = EdgeSet.null $ Graph.edges graph
    neighbours v (UGraph m) = fromMaybe S.empty (M.lookup v m)

uGraph :: UEdgeSet -> UGraph
uGraph es = UGraph (adjacencyList (es `EdgeSet.union` (UEdgeSet (S.map swap (EdgeSet.edges es)))) M.empty)

induced :: (UEdgeSet -> UGraph) -> VertexSet -> UGraph -> UGraph
induced f vs m            =
    let inducedEdges = S.filter (VertexSet.edges vs) (EdgeSet.edges (Graph.edges m))
     in f (UEdgeSet inducedEdges)

