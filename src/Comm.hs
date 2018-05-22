module Comm where

import Data.Text hiding (empty)
import Data.Monoid ((<>))
import Control.Monad.Writer

import Internal.Types
import Internal.Utils

-- | A commuting Diagram is a graph where the nodes are strings and labels are strings
type CommG = Graph Text Text Text

type Eval a = Writer CommG a

obj' :: Text -> Eval ()
obj' = tell . node

obj :: Text -> Eval CommG
obj = return . node

arrow :: CommG -> Text -> CommG -> Eval ()
arrow from lbl to = tell $ pEdge' (fL, lbl, toL) $ from <> to
  where fL = nodeLabel from
        toL = nodeLabel to

-- dupWith :: [Text] -> CommG -> CommG
-- dupWith []  acc = CommG
-- dupWith (x:xs) acc =

commTriangle :: Text -> Text -> Text -> Eval ()
commTriangle a b c = do x <- obj a
                        y <- obj b
                        c <- obj c
                        arrow x "f" y
                        arrow y "g" z
                        arrow z "f" x

commAq a b c d = do j <- obj a
                    k <- obj b
                    l <- obj c
                    n <- obj d
                    arrow j "f" k
                    arrow k "f" l
                    arrow l "f" n
                    arrow n "f" j

mono' :: Eval ()
mono' = do
  z <- obj "Z"
  x <- obj "X"
  obj' "Y"
  arrow z "F" x

runEval :: Eval a -> CommG
runEval = execWriter

mono = runEval mono'
