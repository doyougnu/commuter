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
_arrow Morph'{..} =
  withName (_mFrom ^. name) $ \b1 ->
  withName (_mTo ^. name) $ \b2 ->
  atop (arrowBetween' (with & headGap .~ large & tailGap .~ large) (location b1) (location b2)
        <> alignedText 0 1 _mLabel # moveTo (arrLoc b1 b2) # fontSizeL _mfsize)

-- f = M $ mkMph (mkObj "$\\epsilon A$") "f" (mkObj "B") & setL' (0,0) (2,0)
f = M $ mkMph (mkObj "A") "f" (mkObj "B") & setL' (0,0) (2,0)
g = M $ mkMph (mkObj "B") "g" (mkObj "C") & setL' (2,0) (2,-2)
h = M $ mkMph (mkObj "A") "h" (mkObj "C") & setL' (0,0) (2,-2)
i = M $ mkMph (mkObj "A") "i" (mkObj "D") & setL' (0,0) (0,-2)
j = M $ mkMph (mkObj "D") "j" (mkObj "C") & setL' (0,-2) (2,-2)

test :: Comm
test = sqr g f j i
-- test = (m2 |.| m1) |=| (m4 |.| m3)
test' = do
  t <- test
  r <- test
  return $ t |..| r
-- | TODO make |..| that is the current |.| and make |.| type check the domains and codomains. Lets us an Either type for this.
sem' :: Morph -> Diagram B
sem' (M m) = (_node (_mFrom m) <> _node (_mTo m)) # _arrow m
sem' (m :.: n)   | m == n = sem' m
sem' (xs :.: ys) | coDomain ys == domain xs = foldMap sem' [xs, ys]
                 | otherwise = error "Domain and codomain do not match in composition!"
sem' (lhs :=: rhs)
  | lDomain == rDomain && lRange == rRange = foldMap sem' [lhs, rhs]
  | otherwise = error "domain and codomain of equivalence not equivalent!"
  where lDomain = domain lhs
        lRange  = range lhs
        rDomain = domain rhs
        rRange  = range rhs
-- sem' (M lhs) :=: (M rhs)  = sem' lhs <> sem' rhs

sem :: Show a => Either a Morph -> QDiagram B V2 Double Any
sem (Left err) = error . show $ err
sem (Right ms) = sem' ms
