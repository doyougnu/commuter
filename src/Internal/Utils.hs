module Utils where

import Control.Arrow (first, second)
import Data.Monoid   ((<>))
import Data.Map      (singleton)

import Types
import Core

-- | Given a plain edge and a graph, add that edge to the graph
pEdge :: (Ord n, Ord l) => Edge n l -> Graph n m l -> Graph n m l
pEdge e = G . first (addT e) . unG

-- | a hyper edge is from a edge to and edge
hEdge :: (Ord l) => Edge l m -> Graph n m l -> Graph n m l
hEdge e = G . second (addT e) . unG

-- | Compose to edges
o :: Monoid l => Edge n l -> Edge n l -> Edge n l -- this is monoidic
o (_, ll, d) (a, l, _) = (a, ll <> l, d)

-- Do I really want to constrain edge labels to strings like this?
o' :: Show l => Edge n l -> Edge n l -> Edge n String
o' (_, ll, d) (a, l, _) = (a, show ll ++ " o " ++ show l, d)

-- | A node is just a singleton graph with no edges
node :: (Ord n, Ord l) => n -> Graph n m l
node k = G (singleton k [], empty)

-- | An id edge is a edge from a node to itself with the label id
_idEdge :: (IsString l) => n -> Edge n l
_idEdge n = (n, "id", n)

-- | Given a node, and a graph, create a id edge and add it to the graph
idEdge :: (Ord n, Ord m, Ord l, IsString l) => n -> Graph n m l -> Graph n m l
idEdge = pEdge . _idEdge
