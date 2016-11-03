
module AdjacencyList where 

import qualified Data.Set as S
import qualified Data.Map as M

import Types
import EdgeSet
import VertexSet

type AdjacencyList = (Map Vertex VertexSet)

adjacencyList :: DEdgeSet -> AdjacencyList -> AdjacencyList
adjacencyList es m = 
    let map' = adjacencyList' (EdgeSet.edges es) m
        keys = S.fromList $ M.keys map'
        singletons = EdgeSet.vertices es `S.difference` keys
     in addSingletons map' singletons
    where
        addSingletons = S.foldl (\m' v -> M.insert v S.empty m')
        adjacencyList' es m 
            | S.null es = m
            | otherwise = 
                let (u, v) = S.elemAt 0 es
                in case M.lookup u m of 
                    Nothing -> adjacencyList' (S.deleteAt 0 es) $ M.insert u (S.singleton v) m
                    Just vs -> adjacencyList' (S.deleteAt 0 es) $ M.insert u (S.insert v vs) m

edges :: AdjacencyList -> S.Set Edge
edges m
    | M.null m = S.empty
    | otherwise = 
        let (v, nbs) = M.elemAt 0 m 
         in S.union (S.map (\x -> (v, x)) nbs) (AdjacencyList.edges $ M.deleteAt 0 m)

