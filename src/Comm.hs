module Comm where

import Data.Text hiding (empty)
import Control.Monad.State
import Data.Monoid ((<>))

import Internal.Types
import Internal.Utils

-- | A commuting Diagram is a graph where the nodes are strings and labels are strings
type CommG = Graph Text Text Text

type Eval a m = State (CommG) a

obj :: Text -> Eval (CommG) m
obj = return . node

arrow :: CommG -> Text -> CommG -> Eval (CommG) m
arrow from lbl to = return $ pEdge' (fL, lbl, toL) $ from <> to
  where fL = nodeLabel from
        toL = nodeLabel to

-- dupWith :: [Text] -> CommG -> CommG
-- dupWith []  acc = CommG
-- dupWith (x:xs) acc =

mono' = do
  obj "Z"
  obj "X"
  obj "Y"

runEval :: Eval a m -> CommG
runEval = flip execState mempty

mono = runEval mono'
