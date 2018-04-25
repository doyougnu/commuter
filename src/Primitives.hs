module Primitives where

-- import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.TwoD.Arrow

-- node :: String -> Diagram B
-- node name = text name # fontSizeL 0.2 <> phantom (square 0.25 :: Diagram B) # named name

-- tournament :: Int -> Diagram B
-- tournament n = atPoints (trailVertices $ regPoly n 1) (map node ["A", "B", "C"])
--                # connectOutside ("A" :: String) ("B" :: String)

-- type Obj = String -> Diagram B

-- Named arrows are hard:
-- │18:37:26 byorgey | doyougnu: yeah, this is something
--   that would be really nice, and I'd like to see us tackle it at some point.
--   But I think some │ │ | major redesign might have to happen first. Arrows
--   are complicated.

import Data.Map
import Control.Arrow                  ((&&&), (***))
import Data.Bifunctor                 (bimap, first, second)

type Table n a = Map n a                 -- ^ an adjacency table via nodes

type Adj n l = Table n [(l, n)]        -- ^ node lbls n, func lbls l
type EAdj l m = Table l [(m, l)]       -- ^ func labels l, hfunc lbls m

type Graph n m l = (Adj n l, EAdj l m) -- ^ this graph can have arrows
                                         -- ^ between nodes and arrows
                                         -- ^ between edges

type Edge n l = (n, l, n)                 -- ^ abstracted edges, w/ labels

-- | Label is a function from Nodes to Labels
type Label n l = n -> l
data LGraph nl n m l = LGraph (Graph n m l) (Label nl l)

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
buildTable :: Ord n => [Edge n l] -> Map n [(l, n)]
buildTable = buildBy fst_ sndAndThrd
  where fst_ (a, _, _) = a
        sndAndThrd (_, b, c) = (b, c)

-- | take a map and unwind it, this does Map Node Edges -> [(Node, label, Node2)]
unBuildBy :: (t -> a -> b) -> Map t [a] -> [b]
unBuildBy f = foldrWithKey (\frm ex acc -> ex >>= flip (:) acc . f frm) []

-- unBuildT :: Adj n l -> [Edge n l]
unBuildT :: Map n [(l, n)] -> [Edge n l]
unBuildT = unBuildBy toTrip
  where toTrip a (b, c) = (a, b, c)

-- | build a graph with bounds and a list of edges
buildGraph :: (Ord n, Ord l) => [Edge n l] -> [Edge l m] -> Graph n m l
buildGraph es hs = (buildTable es, buildTable hs)

-- | An empty graph
emptyGraph :: Graph n m l
emptyGraph = (empty, empty)

-- | Given a projection of a graph, and a graph return all the edges in the
-- graph by the provided function
-- OH GOD THIS GENERATED TYPE
edgesBy :: (([Edge n1 l1], [Edge n2 l2]) -> t)
                 -> (Adj n1 l1, EAdj n2 l2) -> t
edgesBy f g = f $ unBuildT *** unBuildT $ g

-- | Get all the plain edges in a graph
plainEdges :: Graph n m l -> [Edge n l]
plainEdges = edgesBy fst

-- | Get all the hyper edges in a graph
hyperEdges :: Graph n m l -> [Edge l m]
hyperEdges = edgesBy snd

-- | decompose teh graph into just a list of edges
decomposeGraph :: Graph n m l -> ([Edge n l], [Edge l m])
decomposeGraph = plainEdges &&& hyperEdges

-- | Flip all the arrow directions in a graph
coGraph :: (Ord n, Ord l) => Graph n m l -> Graph n m l
coGraph g = buildGraph es hs
  where (es, hs) = bimap eFlip eFlip $ decomposeGraph g
        eFlip xs = [ (to, l, fr) | (fr, l, to) <- xs ]

addT :: (Ord n) => Edge n l -> Map n [(l, n)] -> Map n [(l, n)]
addT (from, lbl, to) mp
  | from `member` mp = adjust ((:) (lbl, to)) from mp
  | otherwise = insert from [(lbl, to)] mp

addPEdge :: (Ord n, Ord l) => Edge n l -> Graph n m l -> Graph n m l
addPEdge = first . addT

addHEdge :: (Ord l) => Edge l m -> Graph n m l -> Graph n m l
addHEdge = second . addT
