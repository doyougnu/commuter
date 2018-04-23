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

-- | From Issue 5 of the Monad Reader (Practical Graph Handling) This
-- formulation of a graph is based on generalized recursion schemes so
-- operations on the graph should roughly be an anamorphism (unfold) or a
-- catamorphism (fold) this differs from the tying-the-knot approach to cycle
-- structures
type Vertex = Int
type Table a = Array Vertex a
type Graph e = Table [(e, Vertex)]
type Bounds = (,) Vertex Vertex
type Edge e = (Vertex, e, Vertex)
type FEdge a e = (Edge a, e, Edge a)

type Label a = Vertex -> a
data LGraph n e = LGraph (Graph e) (Label n)

type Node = Int
type Table a = Array Node a
type Graph e = Table [()]
