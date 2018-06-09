module Sem where

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude hiding ((<>), tri, under)

import Data.Monoid                    ((<>))
import Data.Maybe                     (fromJust, isNothing)
import Data.Map                       ((!))
import Control.Monad.State            (runState)
import Control.Monad.Except           (runExceptT)

import Internal.Types
import Internal.Core
import Internal.Position

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


__node :: Obj -> Diagram B
__node Obj{..}
  | isNothing _oPos = def'
  | otherwise = def' # moveTo (p2 (cx,cy))
  where def' = text _name # fontSizeL _fSize
               <> phantom (square 0.25 :: Diagram B)
               # named _name
        (Loc' cx cy)= fromJust _oPos

_node :: Obj -> Diagram B
_node a@Obj{..} = __node b
  where b = foldr ($) a _customizations

-- _arrow :: Morph' -> Diagram B
_arrow Morph{..} =
  withName (_mFrom) $ \b1 ->
  withName (_mTo) $ \b2 ->
  atop (arrowBetween' (with & headGap .~ large & tailGap .~ large) (location b1) (location b2)
        <> alignedText 0 1 _mLabel # moveTo (arrLoc b1 b2) # fontSizeL _mfsize)

-- f = M $ mkMph (mkObj "$\\epsilon A$") "f" (mkObj "B") & setL' (0,0) (2,0)
f' :: Sem Morph
f' = mkMph "A" "f" ("B")

g' :: Sem Morph
g' = mkMph "B" "g" "C"

f :: Sem Morph
f = do m <- mkMph "A" "f" "B"
       setML (0,0) (2,0) m
       return m

g :: Sem Morph
g = do m <- mkMph "B" "g" "C"
       setML (2,0) (2,-2) m
       return m

h :: Sem Morph
h = do m <- mkMph "A" "h" "C"
       setML (0,0) (2,-4) m
       return m

i :: Sem Morph
i = mkMph ("A'") "i" ("C")

j :: Sem Morph
j = mkMph ("A'") "j" ("B")

t1 :: Sem Equ
t1 = tri (g >>= liftToComp) (f >>= liftToComp) (h >>= liftToComp)

t2 :: Sem Equ
t2 = tri (g >>= liftToComp) (j >>= liftToComp) (i >>= liftToComp)

-- test' :: Sem Equ
-- test' = t1 `underE` t2

test' :: Sem Equ
test' = t1

-- test = (m2 |.| m1) |=| (m4 |.| m3)
-- test' = do
--   t <- test
--   r <- test
--   return $ t |==| r

sem' :: Morph -> PosMap -> Diagram B
sem' m objs = (_node fromObj <> _node toObj) # _arrow m
  where from_ = _mFrom m
        to_   = _mTo m
        fromObj = objs ! from_
        toObj   = objs ! to_

-- sem :: Sem Equ -> QDiagram B V2 Double Any
-- sem (Left err) = error . show $ err
-- sem (Right ms) = foldMap (foldMap sem') ms

sem :: Sem Equ -> QDiagram B V2 Double Any
sem = sem'' . flip runState emptySt . runExceptT

sem'' :: (Either ErrMsg Equ, PosMap) -> QDiagram B V2 Double Any
sem'' (Left err, _) = error . show $ err
sem'' (Right ms, objs) = foldMap (foldMap $ flip sem' objs) ms
