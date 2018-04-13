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
