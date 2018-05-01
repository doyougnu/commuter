module Main where

-- import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
-- import Primitives

-- Named arrows are hard:
-- │18:37:26 byorgey | doyougnu: yeah, this is something
--   that would be really nice, and I'd like to see us tackle it at some point.
--   But I think some │ │ | major redesign might have to happen first. Arrows
--   are complicated.

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Arrow

node :: String -> Diagram B
node name = text name # fontSizeL 0.2 <> phantom (square 0.25 :: Diagram B) # named name

-- arrow_ :: Diagram B -> String -> Diagram B -> Diagram B
-- arrow_ f lbl t = f ||| arr ||| t
--   where arr = text lbl
--               # fontSizeL 0.2 <> phantom (rect 1.25 1.50 :: Diagram B)
--               # named lbl
--               # connectOutside ("a" :: String) ("b" :: String)
--         (nf, _) = head $ names f
--         (nt, _) = head $ names t

arrowUnit :: String -> Diagram B
arrowUnit lbl = arr <> txt
  where arr = phantom (rect 2 1 :: Diagram B) <> arrowAt unit_X (2 * unitX) # fontSizeL 0.2
        txt = alignedText 0 1 lbl # fontSizeL 0.2

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices $ regPoly n 1) (map node ["A", "B", "C", "D"])
               # connectOutside ("A" :: String) ("B" :: String)
               # connectOutside ("C" :: String) ("D" :: String)

test :: Diagram B
test = arrowUnit "F"

-- run like stack exec -- commuter -w 400 -h 400 -o test.svg
main :: IO ()
main = mainWith $ test # pad 3
-- main = return ()
