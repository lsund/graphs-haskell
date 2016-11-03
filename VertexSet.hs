
module VertexSet where 

import qualified Data.Set as S

import Types

type VertexSet = S.Set Vertex

edges :: VertexSet -> Edge -> Bool
edges xs (x, y) = all (`S.member` xs) [x, y]

