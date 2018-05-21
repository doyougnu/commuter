module Main where

-- import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
-- import Primitives

-- Named arrows are hard:
-- │18:37:26 byorgey | doyougnu: yeah, this is something
--   that would be really nice, and I'd like to see us tackle it at some point.
--   But I think some │ │ | major redesign might have to happen first. Arrows
--   are complicated.

import Young (toDiagrams, isomorphism, Graph, test)
import Data.Map (empty)
import Diagrams.Prelude hiding (from, to)
import Diagrams.Backend.SVG.CmdLine
import Graphics.SVGFonts
-- import Diagrams.TwoD.Arrow

import Data.Typeable (Typeable)
import Data.Maybe (isJust)

type Loc = (Double, Double)

data Obj = O { name :: String
               , oLoc :: Maybe Loc
               }

data Arr  = A { from :: Obj
                , aName :: String
                , to :: Obj
                , aLoc :: Maybe Loc
                }

data Element = Obj Obj | Arr Arr

-- class Locatable a where
--   loc_ :: a -> Maybe Loc

--   isLocated :: a -> Bool
--   isLocated = isJust . loc_

--   getX :: a -> Maybe Double
--   getX = fmap fst . loc_

--   getY :: a -> Maybe Double
--   getY = fmap snd . loc_

-- instance Locatable Obj where loc_ = oLoc
-- instance Locatable Arr where loc_ = aLoc

-- text' :: (Read n, Renderable (Path V2 n) b, RealFloat n, Typeable n) => n -> String -> QDiagram b V2 n Any
-- text' d s = (strokeP $ textSVG' (TextOpts lin2 INSIDE_H KERN False d d) s) # lw none # fc black

-- obj :: String -> Maybe Loc -> Obj
-- obj = O

-- arrow_ :: Obj -> String -> Obj -> Maybe Loc -> Arr
-- arrow_ = A

-- _node :: Obj -> Diagram B
-- _node O{name=nm, oLoc=(Just l)} = text nm #
--                                   fontSizeL 0.2 <> phantom (square 0.25 :: Diagram B)
--                                   # named nm # moveTo (p2 l)
-- _node O{name=nm, oLoc=Nothing} = text nm #
--                                  fontSizeL 0.2 <> phantom (square 0.25 :: Diagram B)
--                                   # named nm

-- arrowLabel = text' 0.3

-- mkArrowCoord :: (Num a, Num b, Fractional a, Fractional b) => (a, b) -> (a, b) -> (a, b)
-- mkArrowCoord (fx, fy) (tx, ty) =( (fx + tx) / 2
--                                 , ((fy + ty) / 2) + 0.2)

-- __arrow :: Arr -> Diagram B
-- __arrow A{from=f, aName=l, to=t, aLoc=(Just al)} = position (zip points objs)
--                                                    # connectOutside fname tname
--   where (Just (fx, fy)) = loc_ f
--         (Just (tx, ty)) = loc_ t
--         fname = name f
--         tname = name t
--         points = p2 <$> [ (fx, fy)
--                         , al
--                         , (tx, ty)]
--         objs = [_node f, arrowLabel l, _node t]
-- __arrow A{from=f, aName=l, to=t, aLoc=Nothing} = position (zip points objs)
--                                                    # connectOutside fname tname
--   where (Just fc) = loc_ f
--         (Just tc) = loc_ t
--         fname = name f
--         tname = name t
--         points = p2 <$> [fc, mkArrowCoord fc tc, tc]
--         objs = [_node f, arrowLabel l, _node t]

                   -- # connectOutside (fst . head $ names fr) (fst . head $ names t)
-- toDGram :: Element -> Diagram B
-- toDGram (Obj{name=n}) = node_ n
-- toDGram (Arr{from=f, aName=n, to=t}) = _arrow

-- simple :: [Element] -> Diagram B
-- simple xs = position $ zip points dgrams
--   where points = p2 . loc_ <$> xs
--         dgrams = toDgram <$> xs


-- test :: Diagram B
-- test = __arrow $ arrow_ (obj "A" Nothing) "f" (obj "B" Nothing) Nothing

-- run like stack exec -- commuter -w 400 -h 400 -o test.svg
main :: IO ()
-- main = mainWith $ toDiagrams (isomorphism :: Graph Int Int String) empty empty # pad 3
main = mainWith $ allRDs

mySquare :: Diagram B
mySquare = square 2 # fc blue # showOrigin

myCircle :: Diagram B
myCircle = circle 1 # fc brown # showOrigin

myTri :: Diagram B
myTri = triangle 1 # showOrigin

allDs :: Diagram B
allDs = mconcat [myTri, myCircle, mySquare]

allRDs :: Diagram B
allRDs = mconcat $ reverse [myTri, myCircle, mySquare]
-- main = return ()
