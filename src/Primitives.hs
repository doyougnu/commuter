module Primitives where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow

node :: String -> Diagram B
node name = text name # fontSizeL 0.2 <> phantom (square 0.25 :: Diagram B) # named name

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices $ regPoly n 1) (map node ["A", "B", "C"])
               # connectOutside ("A" :: String) ("B" :: String)

type Obj = String -> Diagram B

-- Named arrows are hard:
-- │18:37:26 byorgey | doyougnu: yeah, this is something
--   that would be really nice, and I'd like to see us tackle it at some point.
--   But I think some │ │ | major redesign might have to happen first. Arrows
--   are complicated.
