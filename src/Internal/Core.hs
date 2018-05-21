module Core where

import Diagrams.Prelude hiding (from, to, fc, (<>), adjust)

import Data.Map                       ( empty
                                      , adjust
                                      , member
                                      , insert
                                      , foldrWithKey
                                      )
import Prelude hiding                 (lookup)
import Control.Arrow                  ((&&&), (***))

import Types

-- | wonder which recursion scheme this is
buildBy :: Ord k => (t -> k) -> (t -> a) -> [t] -> Map k [a]
buildBy _    _     [] = empty
buildBy fkey fedge xs = go xs empty
  where
    go [] mp = mp
    go (y:ys) mp
      | key `member` mp = go ys $ adjust ((:) edge) key mp
      | otherwise = go ys $ insert key [edge] mp
      where edge = fedge y
            key = fkey y

-- | Build an adjacency list for nodes
buildTable :: Ord n => [Edge n l] -> Adj n l
buildTable = buildBy fst_ sndAndThrd
  where fst_ (a, _, _) = a
        sndAndThrd (_, b, c) = (b, c)

-- | take a map and unwind it, this does Map Node Edges -> [(Node, label, Node2)]
unBuildBy :: (t -> a -> b) -> Map t [a] -> [b]
unBuildBy f = foldrWithKey (\frm ex acc -> ex >>= flip (:) acc . f frm) []

unBuildT :: Adj n l -> [Edge n l]
unBuildT = unBuildBy toTrip
  where toTrip a (b, c) = (a, b, c)

-- | build a graph with bounds and a list of edges
buildGraph :: (Ord n, Ord l) => [Edge n l] -> [Edge l m] -> Graph n m l
buildGraph es hs = G (buildTable es, buildTable hs)

-- | An empty graph
emptyGraph :: (Ord n, Ord l) => Graph n m l
emptyGraph = mempty

-- | Given a projection of a graph, and a graph return all the edges in the
-- graph by the provided function
-- OH GOD THIS GENERATED TYPE
edgesBy :: (([Edge n1 l1], [Edge n2 l2]) -> t) -> (NAdj n1 l1, EAdj n2 l2) -> t
edgesBy f g = f $ unBuildT *** unBuildT $ g

-- | Get all the plain edges in a graph
plainEdges :: Graph n m l -> [Edge n l]
plainEdges = edgesBy fst . unG

-- | Get all the hyper edges in a graph
hyperEdges :: Graph n m l -> [Edge l m]
hyperEdges = edgesBy snd . unG

-- | decompose teh graph into just a list of edges
decomposeGraph :: Graph n m l -> ([Edge n l], [Edge l m])
decomposeGraph = plainEdges &&& hyperEdges

-- | Flip all the arrow directions in a graph
coGraph :: (Ord n, Ord l) => Graph n m l -> Graph n m l
coGraph g = buildGraph es hs
  where (es, hs) = bimap eFlip eFlip $ decomposeGraph g
        eFlip xs = [ (to, l, fr) | (fr, l, to) <- xs ]

-- | add an edge to a table
addT :: (Ord n) => Edge n l -> Adj n l -> Adj n l
addT (from, lbl, to) mp
  | from `member` mp = adjust ((:) (lbl, to)) from mp
  | otherwise = insert from [(lbl, to)] mp
