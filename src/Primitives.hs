module Primitives where

import Data.Map
import Control.Arrow                  ((&&&), (***), (>>>))
import Data.Bifunctor                 (bimap, first, second)
import Data.Monoid                    ((<>))
import Data.String                    (IsString)


type Table n a = Map n a

type Adj n l = Table n [(l, n)]        -- ^ an adjacency table via ns
type NAdj n l = Adj n l                -- ^ node lbls n, func lbls l
type EAdj l m = Adj l m                -- ^ func labels l, hfunc lbls m

newtype Graph n m l = G { unG :: (Adj n l, EAdj l m)} -- ^ this graph can have arrows
  deriving (Show,Eq)                                  -- ^ between nodes and arrows
                                                      -- ^ between edges

type Edge n l = (n, l, n)                   -- ^ abstracted edges, w/ labels

-- | Label is a function from Nodes to Labels
type Label n l = n -> l
data LGraph nl n m l = LGraph (Graph n m l) (Label nl l)

instance (Ord n, Ord l) => Monoid (Graph n m l) where
  mempty = G (empty, empty)
  mappend (G (a, b)) (G (c, d)) = G (a <> c, b <> d)

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

isomorphism :: (Ord m, Ord n, Ord l, Num n, IsString l) => Graph n m l
isomorphism = idEdge 1 .
              idEdge 2 .
              pEdge (1, "f", 2) .
              pEdge (2, "g", 1) $ node 1 <> node 2

-- Or using Arrows
isomorphism2 :: (Ord m, Ord n, Ord l, Num n, IsString l) => Graph n m l
isomorphism2 = pEdge (1, "f", 2) >>>
               pEdge (2, "g", 1) >>>
               idEdge 1 >>>
               idEdge 2 $ mconcat [node 1, node 2]

sumNodes :: Num n => Graph n m l -> n
sumNodes = Prelude.foldr (+) 0 . keys . fst . unG

-- type SemDiag n m l = Graph n m l -> Diagram B
type SemGraphViz n m l = Graph n m l -> String

toGraphVizNode :: Show a => a -> String
toGraphVizNode n = show n ++ "[label = " ++ show n ++  "];\n"

toGraphVizEdge :: (Show a, Show b, Show c) => (a, [(b, c)]) -> String
toGraphVizEdge (frm, xs) = helper xs
  where helper [] = "\n"
        helper ((lbl, to):ys) =
          show frm ++ " -> " ++ show to ++
          "[label = \"" ++ show lbl ++ "\"];\n" ++ helper ys


toGraphViz :: (Show l, Show n) => String -> SemGraphViz n m l
toGraphViz name (G (n, _)) =
  "digraph " ++ name ++ "{\n" ++
  "rankdir=LR2;\n" ++ concatMap toGraphVizNode (keys n)
  ++ concatMap toGraphVizEdge (assocs n) ++ "}\n"

data F n = F (F n) | Empty deriving Show

even_ :: F n -> Bool
even_ Empty = True
even_ (F n) = odd_ n

odd_ :: F n -> Bool
odd_ Empty = False
odd_ (F n) = even_ n
