module Sem where

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude hiding ((<>), tri)

import Data.Monoid                    ((<>))
import Data.Maybe                     (fromJust, isNothing)

import Internal.Types
import Internal.Core

-- | The semantic function for Graphviz has the semantic domain of strings
-- type SemGraphViz n m l = Graph n m l -> String

-- toGraphVizNode :: Show a => a -> String
-- toGraphVizNode n = show n ++ "[label = " ++ show n ++  "];\n"

-- toGraphVizEdge :: (Show a, Show b, Show c) => (a, [(b, c)]) -> String
-- toGraphVizEdge (frm, xs) = helper xs
--   where helper [] = "\n"
--         helper ((lbl, to):ys) =
--           show frm ++ " -> " ++ show to ++
--           "[label = \"" ++ show lbl ++ "\"];\n" ++ helper ys


-- toGraphViz :: (Show l, Show n) => String -> SemGraphViz n m l
-- toGraphViz name (G (n, _)) =
--   "digraph " ++ name ++ "{\n" ++
--   "rankdir=LR2;\n" ++ concatMap toGraphVizNode (keys n)
--   ++ concatMap toGraphVizEdge (assocs n) ++ "}\n"

arrLoc :: (Fractional a, Additive v) => Subdiagram b1 v a m1 -> Subdiagram b2 v a m2 -> Point v a
arrLoc (location -> _p1) (location -> _p2) = _p1 .+^ vec
  where
    vec = (_p2 .-. _p1) ^/ 2


_node :: Obj -> Diagram B
_node Obj{..}
  | isNothing _oPos = def'
  | otherwise = def' # moveTo (p2 (cx,cy))
  where def' = text _name # fontSizeL _fSize
               <> phantom (square 0.25 :: Diagram B)
               # named _name
        (Loc' cx cy)= fromJust _oPos

-- _arrow :: Morph' -> Diagram B
_arrow Morph{..} =
  withName (_mFrom ^. name) $ \b1 ->
  withName (_mTo ^. name) $ \b2 ->
  atop (arrowBetween' (with & headGap .~ large & tailGap .~ large) (location b1) (location b2)
        <> alignedText 0 1 _mLabel # moveTo (arrLoc b1 b2) # fontSizeL _mfsize)

-- f = M $ mkMph (mkObj "$\\epsilon A$") "f" (mkObj "B") & setL' (0,0) (2,0)
f :: Comm Comp
f = mkMph (mkObj "A") "f" (mkObj "B") & _Right . ix 0 %~ setL' (0,0) (2,0)
g = mkMph (mkObj "B") "g" (mkObj "C") & _Right . ix 0 %~ setL' (2,0) (2,-2)
h = mkMph (mkObj "C") "h" (mkObj "D") & _Right . ix 0 %~ setL' (0,0) (2,-2)
i = mkMph (mkObj "A'") "i" (mkObj "C") & _Right . ix 0 %~ setL' (4,0) (2,-2)
j = mkMph (mkObj "A'") "j" (mkObj "B") & _Right . ix 0 %~ setL' (4,0) (2,0)

t1 :: Comm Equ
t1 = tri g f h

t2 = tri g j i

test' = t1 `join` t2
-- test = (m2 |.| m1) |=| (m4 |.| m3)
-- test' = do
--   t <- test
--   r <- test
--   return $ t |==| r

-- | TODO make |..| that is the current |.| and make |.| type check the domains and codomains. Lets us an Either type for this.
sem' :: Morph -> Diagram B
sem' m = (_node (_mFrom m) <> _node (_mTo m)) # _arrow m

sem :: Comm Equ -> QDiagram B V2 Double Any
sem (Left err) = error . show $ err
sem (Right ms) = foldMap (foldMap sem') ms
