module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Primitives

-- run like stack exec -- commuter -w 400 -h 400 -o test.svg

main :: IO ()
main = mainWith $ tournament 3 # pad 3
