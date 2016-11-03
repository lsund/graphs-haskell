
module Tree where

import Data.List
import qualified Data.Set as S
import qualified Data.Dequeue as Q


import VertexSet
import EdgeSet
import Types
import Graph

type Forest = S.Set Tree
data Tree = Tree { root :: Vertex , children :: Forest } 
          | Empty deriving (Eq, Ord)

instance Show Tree where
    show (Tree x children) = 
        "T:" ++ show x ++ " [" ++ intercalate ", " ( S.toList (S.map show children)) ++ "]"
    show Tree.Empty      = "E"

instance Graph Tree where
    empty       = Tree.Empty
    singleton v = Tree v S.empty
    vertices    = vertices'
    edges       = edges'
    neighbours  = neighbours'
    null        = null'

vertices' :: Tree -> VertexSet
vertices' (Tree x children) = S.foldl (\acc c -> (S.union (vertices' c) acc)) (S.singleton x) children
vertices' Tree.Empty              = S.empty

edges' :: Tree -> DEdgeSet
edges' (Tree x children) =
    let incident = S.foldl (\acc (Tree x' _) -> S.insert (x, x') acc) S.empty children 
     in DEdgeSet $ S.foldl (\acc c -> (S.union (EdgeSet.edges (edges' c)) acc)) incident children
edges' Tree.Empty = DEdgeSet S.empty

neighbours' :: Vertex -> Tree -> VertexSet
neighbours' x tree = 
    let stree = subTree x tree 
     in S.foldl (\acc c -> S.insert (root c) acc) S.empty (children stree)

-- Get the sub tree at label x
subTree :: Vertex -> Tree -> Tree
subTree x Tree.Empty = Tree.Empty 
subTree x tree
  | root tree == x = tree
  | otherwise      = S.elemAt 0 $ S.foldl (\acc c -> S.insert (subTree x c) acc) (S.singleton Tree.Empty) (children tree)

null' :: Tree -> Bool
null' tree = tree == Tree.Empty

-- Insert the first tree in the second tree under label x
insert :: Tree -> Tree -> Vertex -> Tree 
insert tree Tree.Empty _ = tree
insert tree (Tree x' children) x
  | x == x' = 
      if S.null children 
         then Tree x' (S.singleton tree)
         else Tree x' (children `S.union` S.singleton tree)
  | otherwise = Tree x' (S.map (\c -> Tree.insert tree c x) children)

fromEdgeQueue :: Queue Edge -> Tree
fromEdgeQueue queue = 
    case Q.popFront queue of 
      Just ((u, v), _) -> 
            foldl (\tree (u', v') -> Tree.insert (Graph.singleton v') tree u') (Graph.singleton u) queue
      Nothing -> Tree.Empty

levels :: Tree -> [[Int]]
levels = levelStart 0
    where
      levelStart n tree = 
          case level n tree of
          [] -> []
          xs -> xs : levelStart (succ n) tree
      level _ Tree.Empty             = []
      level 0 (Tree x children) = [x]
      level n (Tree x children) = concat $ S.toList $ S.map (level (n - 1)) children

