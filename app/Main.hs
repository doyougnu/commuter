module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


-- run like stack exec -- commuter -w 100 -h 100 -o test.svg
c :: Diagram B
c = circle 1

main :: IO ()
main = mainWith c
