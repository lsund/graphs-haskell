
module EdgeSet where 

import qualified Data.Set as S

import VertexSet
import Types

data UEdgeSet = UEdgeSet (S.Set Edge)
data DEdgeSet = DEdgeSet (S.Set Edge)

class EdgeSet a where
    edges :: a -> S.Set Edge
    vertices :: a -> VertexSet
    null :: a -> Bool

instance EdgeSet UEdgeSet where

    edges (UEdgeSet es) = es

    vertices set = S.foldl (\acc (u, v) -> (S.fromList [u, v] `S.union` acc)) S.empty (EdgeSet.edges set) 

    null set = S.null es where es = EdgeSet.edges set

instance EdgeSet DEdgeSet where 

    edges (DEdgeSet es) = es

    vertices set = S.foldl (\acc (u, v) -> (S.fromList [u, v] `S.union` acc)) S.empty (EdgeSet.edges set) 

    null set = S.null es where es = EdgeSet.edges set

union seta setb  =  DEdgeSet (esa `S.union` esb)
    where esa = EdgeSet.edges seta
          esb = EdgeSet.edges setb
