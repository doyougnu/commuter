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

import Data.Array
import Control.Arrow                  ((&&&))
import Data.Bifunctor                 (bimap, Bifunctor, first)

-- | From Issue 5 of the Monad Reader (Practical Graph Handling) This
-- formulation of a graph is based on generalized recursion schemes so
-- operations on the graph should roughly be an anamorphism (unfold) or a
-- catamorphism (fold) this differs from the tying-the-knot approach to cycle
-- structures
type Table n a = Array n a                 -- ^ an adjacency table via nodes

type Adj n l = Maybe (Table n [(l, n)])

type Graph n m l = (Adj n l, Adj m l) -- ^ this graph can have arrows
                                      -- ^ between nodes and arrows
                                      -- ^ between edges

type Bounds n m = (Maybe (n, n), Maybe (m, m))

type Edge n l = (n, l, n)                 -- ^ abstracted edges, w/ labels

-- | Label is a function from Nodes to Labels
type Label n l = n -> l
data LGraph nl n m l = LGraph (Graph n m l) (Label nl l)

-- | Empty tables
emptyTable :: Adj n l
emptyTable = Nothing

-- | Build an adjacency list
buildA :: (Ix n, Num n) => Maybe (n, n) -> [Edge n l] -> Adj n l
buildA _          []    = Nothing
buildA Nothing    _     = Nothing
buildA (Just nbs) edges = Just $ accumArray (flip (:)) [] nbs
                          [(from , (lbl, to)) | (from, lbl, to) <- edges]

-- | build a graph with bounds and a list of edges
buildGraph :: (Ix n, Ix m, Num n, Num m) =>
  Bounds n m -> [Edge n l] -> [Edge m l] -> Graph n m l
buildGraph (nbs, ebs) es hs = bimap (buildA nbs) (buildA ebs) (es, hs)

-- | Given a projection of a graph, and a graph return all the edges in the
-- graph by the provided function
edgesBy :: Ix n => (t -> Adj n l) -> t -> [Edge n l]
edgesBy f (f-> Just nTab) = [(fr, lbl, to) |
                             fr <- indices nTab, (lbl, to) <- nTab ! fr]
edgesBy _ _               = []

-- | avoid some extra key presses
mkBounds :: n -> n -> m -> m -> Bounds n m
mkBounds n1 n2 m1 m2 = bimap Just Just ((n1, n2), (m1, m2))

-- | Get the bounds of the graph
gBounds :: Graph n m l -> Bounds n m
gBounds = bimap f f
  where f = fmap bounds

-- | Get all the plain edges in a graph
plainEdges :: Ix n => Graph n m l -> [Edge n l]
plainEdges = edgesBy fst

-- | Get all the hyper edges in a graph
hyperEdges :: Ix m => Graph n m l -> [Edge m l]
hyperEdges = edgesBy snd

-- | Not sure which will be more convenient to work with yet
decomposeGraph' :: (Ix n, Ix m) => Graph n m l -> ([Edge n l], [Edge m l])
decomposeGraph' = plainEdges &&& hyperEdges

-- | Flip all the arrow directions in a graph
coGraph :: (Num n, Num m, Ix n, Ix m) => Graph n m l -> Graph n m l
coGraph g = buildGraph (gBounds g) es hs
  where (es, hs) = bimap eFlip eFlip $ decomposeGraph' g
        eFlip xs = [ (to, l, fr) | (fr, l, to) <- xs]

-- | update the cat1 table given a function
updateNBy :: (Functor f, Bifunctor p) => (a -> b) -> p (f a) c -> p (f b) c
updateNBy = first . fmap

addEdge :: (Ix n) => Edge n l -> Graph n m l -> Graph n m l
addEdge (from, l, to) g = updateNBy (\x -> accum (flip (++)) x [(from, [(l, to)])]) g

-- -- Change the representation we need an adjacency list for nodes, and one for edges
