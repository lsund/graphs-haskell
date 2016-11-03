
module Types where

import qualified Data.Map as M
import qualified Data.Dequeue as Q

type Queue = Q.BankersDequeue
type Map = M.Map

type Vertex = Int
type Edge   = (Vertex, Vertex)

