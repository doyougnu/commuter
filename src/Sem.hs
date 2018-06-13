{-# LANGUAGE TemplateHaskell #-}
module Sem where

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude hiding ((<>), tri, under,adjust,at,trace,_x,_y,map)

import Data.Monoid                    ((<>))
import Data.List                      (sort,(\\))
import Data.Maybe                     (catMaybes, fromJust, isNothing)
import Data.Map                       (elems, keys, (!),adjust)
import Control.Monad.State            (execState, runState, State, modify, get)
import Control.Monad.Except           (runExceptT)
import Debug.Trace (trace)


import Internal.Types
import Internal.Core hiding (map, foldr)
import Internal.Position

data TagData = TagData { _xPos :: Double
                       , _yPos :: Double
                       , _posMap :: PosMap
                       , _incX :: Double
                       , _incY :: Double
                       }

type TagState = State TagData


makeLenses ''TagData

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
-- | this type purposefully left as open as possible. Trust me.
arrowSem Morph{..} =
  withName (_mFrom) $ \b1 ->
  withName (_mTo) $ \b2 ->
  atop (arrowBetween' (with
                       & headGap .~ large
                       & headLength .~ verySmall
                       & tailGap .~ large
                       & shaftStyle %~ fmat []
                      . lw veryThin)
         (location b1) (location b2) <> alignedText 0 1 _mLabel
         # moveTo (arrLoc b1 b2) # fontSizeL _mfSize)
  where fmat (Unique:_) = dashingG [0.50 , 0.22] 0
        fmat _ = id

_arrow a@Morph{..} = arrowSem b
  where b = foldr ($) a _mCustomizations

-- f = M $ mkMph (mkObj "$\\epsilon A$") "f" (mkObj "B") & setL' (0,0) (2,0)
-- f' :: Sem Morph
-- f' = mkMph "A" "f" "B"

-- g' :: Sem Morph
-- g' = mkMph "B" "g" "C"

-- h' :: Sem Morph
-- h' = mkMph "A" "h" "C"

-- f :: Sem Morph
-- f = do m <- mkMph "A" "f" "B"
--        -- setMLoc (10,0) (12,0) m
--        setOLoc 10 0 "A"
--        setOLoc 12 0 "B"
--        return m

-- g :: Sem Morph
-- g = do m <- mkMph "B" "g" "C"
--        setMLoc (2,0) (2,-2) m
--        return m

-- h :: Sem Morph
-- h = mkMph "C" "h" "C'"

-- i :: Sem Morph
-- i = mkMph "A'" "i" "B'"

-- j :: Sem Morph
-- j = mkMph ("B'") "j" ("C'")

-- t1 :: Sem Comp
-- t1 = tri f g h

-- t2 :: Sem Equ
-- t2 = sqr f' g' h' f

-- t3 :: Sem Comp
-- t3 = do h <- mkMph "C" "h" "C'"
--         i <- mkMph "C" "h" "C'"
--         j <- mkMph "C" "h" "C'"
--         j' <- mkMph "C" "h" "C'"
--         j' |.| j |.| h |.| i

-- t4 :: Sem Equ
-- t4 = (h |.| j) |==| i

-- test' :: Sem Equ
-- test' = t1 `underE` t2

-- test' :: Sem Equ
-- test' = t3 `under` t4

-- test = (m2 |.| m1) |=| (m4 |.| m3)
-- test' = do
--   t <- test
--   r <- test
--   return $ t |==| r

sem' :: Morph -> PosMap -> Diagram B
sem' m objs = (_node fromObj_ <> _node toObj_) # _arrow m
  where from_ = _mFrom m
        to_   = _mTo m
        fromObj_ = objs ! from_
        toObj_   = objs ! to_

sem_ :: Sem Equ -> QDiagram B V2 Double Any
sem_ = sem'' . flip runState emptySt . runExceptT

sem'' :: (Either ErrMsg Equ, PosMap) -> QDiagram B V2 Double Any
sem'' (Left err, _) = error . show $ err
sem'' (Right ms, objs) = foldMap (foldMap $ flip sem' objs) ms

validateObjs :: (Either ErrMsg Equ, PosMap) -> (Either ErrMsg Equ, PosMap)
validateObjs a@(Right ms, objs) | null $ objectNamesE ms \\ keys objs = a
                                | otherwise = (Left . NoObj $ "Object names did not match internal state names, here is the necessary info: " ++ (show $ objectNamesE ms) ++ show objs , objs)
validateObjs a@(Left   _,_) = a

-- | WARNING, PAST THIS POINT IS A HALF IMPLEMENTED, POORLY THOUGHT OUT
-- POSITIONING ENGINE. HERE THERE BE DRAGONS
tagLocations :: (Either ErrMsg Equ, PosMap) -> (Either ErrMsg Equ, PosMap)
tagLocations a@(Left _,_) = a
tagLocations (Right ms, objs) = (Right ms , newMap)
  where emptyTagSt = TagData {_xPos=maxX,_yPos=maxY,_posMap=objs,_incX=2,_incY=(-2)}
        newMap = _posMap $ execState (tagEqu (map sort ms)) emptyTagSt
        positions = catMaybes $ _oPos <$> elems objs
        maxX | null positions = 0
             | otherwise = maximum . fmap _x $ positions
        maxY | null positions = 0
             | otherwise = maximum . fmap _y $ positions

tagObj :: String -> TagState ()
tagObj s = do TagData{..} <- get
              setOLocConst _xPos _yPos s

tagMorph :: Morph -> TagState ()
tagMorph m = do TagData{..} <- get
                tagObj (_mFrom m)
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
tagAndIncY cs = tagComp cs >> incY_

tagEqu :: Equ -> TagState ()
tagEqu = mapM_ tagAndIncY

-- | Set an object's location in the state map if and only if it is not already
-- set
setOLocConst :: Double -> Double -> String -> TagState ()
setOLocConst x_ y_ obj =
  do TagData{..} <- get
     if isNothing . _oPos $ _posMap ! obj
       then forceSetPos x_ y_ obj >> incX_
       else return ()

-- | Special set the range of a morph. This is an special case for equivalences
forceSetPos :: Double -> Double -> String -> TagState ()
forceSetPos x_ y_ o = modify (posMap %~ (adjust (oPos .~ Just (Loc' x_ y_)) o))
