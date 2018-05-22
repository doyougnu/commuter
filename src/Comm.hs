module Comm where

import Data.Text hiding (empty)
import Data.Monoid ((<>))
import Control.Monad.Writer

import Internal.Types
import Internal.Utils

-- | A commuting Diagram is a graph where the nodes are strings and labels are strings
type CommG = Graph Text Text Text

newtype Eval a = E {unE :: Writer CommG a}
  deriving (Functor,Applicative,Monad)

instance Monoid a => Monoid (Eval a) where
  mempty = mempty
  w1 `mappend` w2 = let (a1, s1) = runWriter $ unE w1
                        (a2, s2) = runWriter $ unE w2
                        in E . writer $ (a1 <> a2, s1 <> s2)


obj' :: Text -> Eval ()
obj' = E . tell . node

obj :: Text -> Eval CommG
obj = E . return . node

arrow' :: CommG -> Text -> CommG -> Eval ()
arrow' from lbl to = E . tell $ pEdge' (fL, lbl, toL) $ from <> to
  where fL = nodeLabel from
        toL = nodeLabel to

arrow :: Text -> Text -> Text -> Eval ()
arrow fr lbl to = arrow' (node fr) lbl (node to)

arrow'' :: CommG -> Text -> CommG -> CommG
arrow'' from lbl to = pEdge' (fL, lbl, toL) $ from <> to
  where fL = nodeLabel from
        toL = nodeLabel to

-- dupWith :: [Text] -> CommG -> CommG
-- dupWith []  acc = CommG
-- dupWith (x:xs) acc =

commTriangle :: Text -> Text -> Text -> Eval ()
commTriangle a b c = do x <- obj a
                        y <- obj b
                        z <- obj c
                        arrow' x "f" y
                        arrow' y "g" z
                        arrow' z "f" x

commSq' :: Eval ()
commSq' = do arrow "a" "f" "b"
             arrow "b" "g" "c"
             arrow "c" "h" "d"
             arrow "d" "i" "a"

mono' :: Eval ()
mono' = do
  z <- obj "Z"
  x <- obj "X"
  obj' "Y"
  arrow' z "F" x

runEval :: Eval a -> CommG
runEval = execWriter . unE

mono = runEval mono'
commSq = runEval commSq'
test = runEval $ commSq' <> commTriangle "a" "b" "c"
