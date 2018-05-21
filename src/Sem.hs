module Sem where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (from, to, fc, (<>), adjust)

import Data.Typeable (Typeable)
import Data.String   (IsString, fromString)
import Data.Monoid                    ((<>))
import Data.Map                       ( empty
                                      , lookup
                                      , keys
                                      , assocs
                                      )
import Prelude hiding                 (lookup)

import Primitives

-- | The semantic function for Graphviz has the semantic domain of strings
type SemGraphViz n m l = Graph n m l -> String

toGraphVizNode :: Show a => a -> String
toGraphVizNode n = show n ++ "[label = " ++ show n ++  "];\n"

toGraphVizEdge :: (Show a, Show b, Show c) => (a, [(b, c)]) -> String
toGraphVizEdge (frm, xs) = helper xs
  where helper [] = "\n"
        helper ((lbl, to):ys) =
          show frm ++ " -> " ++ show to ++
          "[label = \"" ++ show lbl ++ "\"];\n" ++ helper ys


toGraphViz :: (Show l, Show n) => String -> SemGraphViz n m l
toGraphViz name (G (n, _)) =
  "digraph " ++ name ++ "{\n" ++
  "rankdir=LR2;\n" ++ concatMap toGraphVizNode (keys n)
  ++ concatMap toGraphVizEdge (assocs n) ++ "}\n"

_node :: (IsString n, IsName n) => n -> LocTable n -> Diagram B
_node nm (lookup nm -> Just l) = text (show nm) # fontSizeL 0.02 <> phantom (square 0.25 :: Diagram B)
                                 # named nm # moveTo (p2 l)
_node nm (lookup nm -> Nothing) = text (show nm)
                                  # fontSizeL 0.02 <> phantom (square 0.25 :: Diagram B)
                                  # named nm
_node _  _                      = mempty

newtype RendLabel = L {unL :: String} deriving (Typeable,Ord,Eq,Show)

instance IsName RendLabel
instance IsString RendLabel where fromString = L

arrLoc :: (Fractional a, Additive v) => Subdiagram b1 v a m1 -> Subdiagram b2 v a m2 -> Point v a
arrLoc (location -> _p1) (location -> _p2) = _p1 .+^ vec
  where
    vec = (_p2 .-. _p1) ^/ 2

_arrow f lbl t = withName f $ \b1 ->
  withName t $ \b2 ->
  atop (arrowBetween' (with & headGap .~ large & tailGap .~ large) (location b1) (location b2)
        <> alignedText 0 1  lbl # moveTo (arrLoc b1 b2) # fontSizeL 0.02)

test :: QDiagram B V2 Double Any
test = ((_node ("A" :: String) empty) ||| (_node ("B" :: String) empty) === _node ("C" :: String) empty)
       # _arrow ("C" :: String) "f" ("A" :: String)

toDiagrams :: Graph n m l -> LocTable n -> LocTable l -> Diagram B
toDiagrams (G (ns, es)) _ _ = regPoly numOs 1
  where numOs = length $ keys ns
