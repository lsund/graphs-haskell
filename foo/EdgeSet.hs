

module EdgeSet where

import Data.Set

type Edge = (Int, Int)
data DirectedEdgeSet = DirectedEdgeSet (Set Edge) Int

class EdgeSet c where
    content :: c -> Set Edge
    size :: c -> Int

instance EdgeSet DirectedEdgeSet where
    content (DirectedEdgeSet es _) = es
    size (DirectedEdgeSet _ x) = x 
    
