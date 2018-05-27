module Sem where

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude hiding ((<>))

-- import Data.Typeable (Typeable)
-- import Data.String   (IsString, fromString)
import Data.Monoid                    ((<>))
import Data.Maybe                     (fromJust, isNothing)
-- import Data.Map                       ( empty
--                                       , lookup
--                                       , keys
--                                       , assocs
--                                       )
-- import Prelude hiding                 (lookup)

import Internal.Types

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

-- _node :: (IsString n, IsName n) => n -> LocTable n -> Diagram B
-- _node nm (lookup nm -> Just l) = text (show nm) # fontSizeL 0.22
--                                  <> phantom (square 0.25 :: Diagram B)
--                                  # named nm # moveTo (p2 l)
-- _node nm (lookup nm -> Nothing) = text (show nm)
--                                   # fontSizeL 0.22
--                                   <> phantom (square 0.25 :: Diagram B)
--                                   # named nm
-- _node _  _                      = mempty

-- newtype RendLabel = L {unL :: String} deriving (Typeable,Ord,Eq,Show)
-- instance IsName RendLabel
-- instance IsString RendLabel where fromString = L

-- arrLoc :: (Fractional a, Additive v) => Subdiagram b1 v a m1 -> Subdiagram b2 v a m2 -> Point v a
-- arrLoc (location -> _p1) (location -> _p2) = _p1 .+^ vec
--   where
--     vec = (_p2 .-. _p1) ^/ 2

-- _arrow f lbl t = withName f $ \b1 ->
--   withName t $ \b2 ->
--   atop (arrowBetween' (with & headGap .~ large & tailGap .~ large) (location b1) (location b2)
--         <> alignedText 0 1 lbl # moveTo (arrLoc b1 b2) # fontSizeL 0.22)

-- test :: QDiagram B V2 Double Any
-- test = ((_node ("A" :: String) empty) ||| (_node ("B" :: String) empty) === _node ("C" :: String) empty)
--        # _arrow ("C" :: String) "f" ("A" :: String)

-- -- toDiagrams :: Graph n m l -> LocTable n -> LocTable l -> Diagram B
-- toDiagrams :: (IsName n, IsString n, Show l) =>
--   Graph n m l -> p1 -> p2 -> QDiagram B V2 Double Any
-- -- toDiagrams (G (ns, es)) _ _ = atPoints (regPoly numOs 1) nodes # arrows
-- toDiagrams (G (ns, es)) _ _ = mconcat nodes -- # arrows
--   where numOs = length $ keys ns

--         flatten (_, []) = []
--         flatten (x, (y, z):zs) = (x, y, z) : flatten (x, zs)

--         xs = concatMap flatten $ assocs ns
--         ks = keys ns

--         nodes = flip _node empty <$> ks

--         -- arrows = mconcat $ (\(x,y,z) -> _arrow x (show y) z) <$> xs


_node :: Obj -> Diagram B
_node Obj{..}
  | isNothing _oPos = def'
  | otherwise = def' # moveTo (p2 (cx,cy))
  where def' = text (show _name) # fontSizeL _fSize
               <> phantom (square 0.25 :: Diagram B)
               # named _name
        (Loc' cx cy)= fromJust _oPos


-- sem :: Comm -> Diagram B
-- sem [] acc = acc
-- sem (x:xs) acc = sem' x <> sem xs
