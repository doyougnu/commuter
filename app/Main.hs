module Main where

-- import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
-- import Primitives

-- Named arrows are hard:
-- │18:37:26 byorgey | doyougnu: yeah, this is something
--   that would be really nice, and I'd like to see us tackle it at some point.
--   But I think some │ │ | major redesign might have to happen first. Arrows
--   are complicated.

import Diagrams.Prelude hiding (from, to, E)
import Diagrams.Backend.PGF.CmdLine
import Data.Text(Text)

import Examples
import Api


-- run like stack exec -- commuter -w 400 -h 400 -o test.svg
instance IsName Text

main :: IO ()
main = mainWith $ sem product2 -- ((sem product2L) ||| (sem product2R)) --`beneath` (sem product_)
-- main = mainWith $ allRDs
