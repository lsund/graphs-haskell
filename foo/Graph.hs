
module Graph where

import EdgeSet

class Graph a where
    edges :: EdgeSet c => a -> c



