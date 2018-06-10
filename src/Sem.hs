{-# LANGUAGE TemplateHaskell #-}
module Sem where

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude hiding ((<>), tri, under,adjust,at,trace)

import Data.Monoid                    ((<>))
import Data.List                      ((\\),sort)
import Data.Maybe                     (fromJust, isNothing)
import Data.Map                       (keys, (!),adjust)
import Control.Monad.State            (execState, runState, State, modify, get)
import Control.Monad.Except           (runExceptT)

import Internal.Types
import Internal.Core
import Internal.Position
import Debug.Trace (trace)

data TagData = TagData { _xPos :: Double
                       , _yPos :: Double
                       , _posMap :: PosMap
                       , _incX :: Double
                       , _incY :: Double
                       }

type TagState = State TagData

makeLenses ''TagData
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
f' = mkMph "A" "f" "B"

g' :: Sem Morph
g' = mkMph "B" "g" "C"

h' :: Sem Morph
h' = mkMph "A" "h" "C"

f :: Sem Morph
f = do m <- mkMph "A" "f" "B"
       setMLoc (0,0) (2,0) m
       return m

g :: Sem Morph
g = do m <- mkMph "B" "g" "C"
       setMLoc (2,0) (2,-2) m
       return m

h :: Sem Morph
h = do m <- mkMph "A" "h" "C"
       setOLoc 0 0 "A"
       setOLoc 2 (-2) "C"
       return m

i :: Sem Morph
i = mkMphAt "A'" "i" "C" (4,0) (2,-2)

j :: Sem Morph
j = mkMphAt ("A'") "j" ("B") (0,0) (2,0)

t1 :: Sem Equ
t1 = tri (g' >>= liftToComp) (f' >>= liftToComp) (h' >>= liftToComp)

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

sem :: Sem Equ -> QDiagram B V2 Double Any
sem = sem'' . tagLocations . validateObjs . flip runState emptySt . runExceptT

sem'' :: (Either ErrMsg Equ, PosMap) -> QDiagram B V2 Double Any
sem'' (Left err, _) = error . show $ err
sem'' (Right ms, objs) = foldMap (foldMap $ flip sem' objs) ms

validateObjs :: (Either ErrMsg Equ, PosMap) -> (Either ErrMsg Equ, PosMap)
validateObjs a@(Right ms, objs) | null $ objectNamesE ms \\ keys objs = a
                                | otherwise = (Left . NoObj $ "Object names did not match internal state names, here is the necessary info: " ++ (show $ objectNamesE ms) ++ show objs , objs)
validateObjs a@(Left   _,_) = a

tagLocations :: (Either ErrMsg Equ, PosMap) -> (Either ErrMsg Equ, PosMap)
tagLocations a@(Left _,_) = a
tagLocations (Right ms, objs) = trace (show newMap ++ "\n Before: \n" ++ show objs ++ " order: \n" ++ show (map sort ms))
                                $ (Right ms , newMap)
  where emptyTagSt = TagData {_xPos=0,_yPos=0,_posMap=objs,_incX=2,_incY=(-2)}
        newMap = _posMap $ execState (tagEqu (map sort ms)) emptyTagSt

tagObj :: String -> TagState ()
tagObj s = do TagData{..} <- get
              setOLocConst _xPos _yPos s

tagMorph :: Morph -> TagState ()
tagMorph m = do TagData{..} <- get
                trace ("Setting: " ++ show m) $ tagObj (_mFrom m)
                tagObj (_mTo m)

incX_ :: TagState ()
incX_ = do TagData{..} <- get
           modify $ xPos %~ (+_incX)

incY_ :: TagState ()
incY_ = do TagData{..} <- get
           modify $ yPos %~ (+_incY)

tagComp :: Comp -> TagState ()
tagComp = mapM_ tagMorph

tagAndIncY :: Comp -> TagState ()
tagAndIncY cs = do tagComp cs
                   TagData{..} <- get
                   forceSetPos _xPos _yPos rng
                   incY_
  where rng = _mTo . head $ cs

tagEqu :: Equ -> TagState ()
tagEqu = mapM_ tagAndIncY

-- | Set an object's location in the state map if and only if it is not already
-- set
setOLocConst :: Double -> Double -> String -> TagState ()
setOLocConst x_ y_ obj =
  do TagData{..} <- get
     if isNothing . _oPos $ _posMap ! obj
       then trace ("Setting: " ++ obj ++ " to " ++ show x_ ++ " : " ++ show y_)
            $ forceSetPos x_ y_ obj >> incX_
       else return ()

-- | Special set the range of a morph. This is an special case for equivalences
forceSetPos :: Double -> Double -> String -> TagState ()
forceSetPos x_ y_ o = modify (posMap %~ (adjust (oPos .~ Just (Loc' x_ y_)) o))
