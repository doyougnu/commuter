module Api ( module Internal.Types
           , module Internal.Core
           , module Internal.Position
           , module Custom
           , module Control.Lens
           , module Sem
           , module Diagrams.Prelude
           , obj
           , objAt
           , objects
           , objectsAt
           , copyWith
           , prime
           , uniqueArr
           , arrow
           , arrowAt
           , connectWithLabels
           , intersperseWithLabels
           , sem
           , coords
           , offset
           , lower
           , raise
           , left
           , right
           , labels
           , labelsBy
           , labelsBetween
           , labelsBetweenBy
           , objectLabels) where

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude hiding ((<>), tri, under,adjust,at,trace,_x,_y,arrow,arrowAt,coords,fc,offset)
import Control.Monad.ListM (zipWithM3)
import Control.Monad       (zipWithM)
import Control.Lens hiding (under, (#),beside,transform,backwards,none,(.>))
import Data.Char (toUpper)

import Internal.Types
import Internal.Core
import Internal.Position
import Custom
import Sem

obj :: String -> Sem String
obj = mkObj

objAt :: String -> (Double,Double) -> Sem String
objAt = mkObjAt

objects :: [String] -> Sem [String]
objects = mapM mkObj

objectsAt :: [String] -> [(Double,Double)] -> Sem [String]
objectsAt = zipWithM objAt

copyWith :: (Obj -> Obj) -> String -> Sem String
copyWith f o = do o' <- getObj o
                  let newO = f o'
                  _ <- insertObj newO
                  return (_name newO)

prime :: String -> Sem String
prime o = copyWith (name %~ flip (++) "'") o

uniqueArr :: String -> String -> String -> Sem Morph
uniqueArr frm lbl to_ = unique <$> mkMph frm lbl to_

arrow :: String -> String -> String -> Sem Morph
arrow = mkMph

arrowAt :: String -> String -> String ->
  (Double,Double) -> (Double,Double) -> Sem Morph
arrowAt f l t fc tc = do a <- arrow f l t
                         _ <- setMLoc fc tc a
                         return a

connectWithLabels :: [String] -> Comp -> Comp -> Sem Equ
connectWithLabels ls os us = newMs `mergeE` ((pure os) `mergeE'` (pure us))
  where
    oObjects = objectNamesC os
    uObjects = objectNamesC us
    newMs = zipWithM3 mkMph ls oObjects uObjects >>= return . pure

intersperseWithLabels :: [String] -> [String] -> Sem Comp
intersperseWithLabels flabs os = zipWithM3 mkMph os flabs os'
  where os' = tail os ++ [head os]

-- | Run Like:
-- run like stack exec -- commuter -w 400 -h 400 -o test.svg
-- with:
-- main :: IO ()
-- main = mainWith $ sem test' # pad 3
-- see examples
sem :: Sem Equ -> QDiagram B V2 Double Any
sem = sem_

coords :: Double -> Double -> Double -> [(Double, Double)]
coords xs ys z = zip [xs,(xs+z)..] [ys,(ys+z)..]

offset :: (Double,Double) -> (Double,Double) -> Sem Comp -> Sem ()
offset frms tos cmp = cmp >>= transC frms tos

lower :: Double -> Sem Comp -> Sem ()
lower a = offset (0,-a) (0,-a)

raise :: Double -> Sem Comp -> Sem ()
raise a = offset (0,a) (0,a)


right :: Double -> Sem Comp -> Sem ()
right a = offset (a,0) (a,0)

left :: Double -> Sem Comp -> Sem ()
left a = offset (-a,0) (-a,0)

labelsBy :: (String -> String) -> Char -> [String]
labelsBy f a = f . (:[]) <$> [a..]

labelsBetweenBy :: (String -> String) -> Char -> Char -> [String]
labelsBetweenBy f a b = f . (:[]) <$> [a..b]

labels :: Char -> [String]
labels = labelsBy id

labelsBetween :: Char -> Char -> [String]
labelsBetween = labelsBetweenBy id

objectLabels :: Char -> Char -> [String]
objectLabels = labelsBetweenBy (fmap toUpper)
