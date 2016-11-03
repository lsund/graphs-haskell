
module FindAugPath where

FindAugPath graph
    | isBipartite graph = FindAugPath' graph
    | otherwise         = error "must be bipartite"

FindAugPath' graph = True

{-constructFlow graph = (-}

