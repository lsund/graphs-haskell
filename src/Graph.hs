
module Graph where

import qualified Data.Set as S
import Data.List

import EdgeSet
import VertexSet
import Types

type Matching = UEdgeSet

class Graph a where
    empty      :: a
    singleton  :: Vertex -> a
    vertices   :: a -> VertexSet
    edges      :: (EdgeSet b) => a -> b
    null       :: a -> Bool
    neighbours :: Vertex -> a -> VertexSet

validMatching :: Matching -> Bool
validMatching matching = 
    let unique = S.toList (EdgeSet.vertices matching)
        all = S.foldl (\acc (x, y) -> [x, y] ++ acc) [] (EdgeSet.edges matching)
     in sort unique == sort all

