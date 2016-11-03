
module Tree where

import Graph
import EdgeSet

data Tree = Tree

instance Graph Tree where
    edges tree = DirectedEdgeSet

