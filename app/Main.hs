module Main where

-- import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
-- import Primitives

-- Named arrows are hard:
-- │18:37:26 byorgey | doyougnu: yeah, this is something
--   that would be really nice, and I'd like to see us tackle it at some point.
--   But I think some │ │ | major redesign might have to happen first. Arrows
--   are complicated.

import Data.Map (empty)
import Diagrams.Prelude hiding (from, to)
import Diagrams.Backend.PGF.CmdLine
import Graphics.SVGFonts

import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import Data.Text(Text)

import Comm
import Sem


-- run like stack exec -- commuter -w 400 -h 400 -o test.svg
instance IsName Text

main :: IO ()
main = mainWith $ toDiagrams five empty empty # pad 3
-- main = mainWith $ allRDs
