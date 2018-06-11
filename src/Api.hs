module Api ( module Internal.Types
           , module Internal.Core
           , module Internal.Position
           , module Custom
           , module Control.Lens
           , module Sem
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
           , coordsX
           , coordsY
           , offsetMorph
           , lowerMorph
           , raiseMorph
           , leftMorph
           , rightMorph
           , labels
           , labelsBy
           , labelsBetween
           , labelsBetweenBy
           , objectLabels
           , offset
           , left
           , right
           , lower
           , raise
           , offsetC
           , leftC
           , rightC
           , lowerC
           , raiseC) where

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

import Debug.Trace(trace)
import Internal.Debug

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
connectWithLabels ls os us = do newMs' <- newMs
                                let oss = newMs' `merge'` os
                                    uss = newMs' `merge'` us
                                uss `mergeE` oss
  where
    oObjects = objectNamesC os
    uObjects = objectNamesC us
    newMs = zipWithM3 mkMph oObjects ls uObjects

intersperseWithLabels :: [String] -> [String] -> Sem Comp
intersperseWithLabels flabs os = zipWithM3 mkMph os flabs os'
  where os' = tail os

-- | Run Like:
-- run like stack exec -- commuter -w 400 -h 400 -o test.svg
-- with:
-- main :: IO ()
-- main = mainWith $ sem test' # pad 3
-- see examples
sem :: Sem Equ -> QDiagram B V2 Double Any
sem = sem_

coordsX :: Double -> Double -> Double -> [(Double, Double)]
coordsX xs _ z = zip [xs,(xs+z)..] $ repeat 0

coordsY :: Double -> Double -> Double -> [(Double, Double)]
coordsY _ xs z = zip (repeat 0) [xs,(xs+z)..]

offsetMorph :: (Double,Double) -> (Double,Double) -> Comp -> Sem ()
offsetMorph = transC

offset :: (Double,Double) -> String -> Sem ()
offset = uncurry transO

lower :: Double -> String -> Sem ()
lower a = offset (0,-a)

right :: Double -> String -> Sem ()
right a = offset (a,0)

left :: Double -> String -> Sem ()
left a = offset (-a,0)

raise :: Double -> String -> Sem ()
raise a = offset (0,a)

offsetC :: (Double,Double) -> Comp -> Sem ()
offsetC a = mapM_ (offset a)  . objectNamesC

lowerC :: Double -> Comp -> Sem ()
lowerC a = offsetC (0,-a)

rightC :: Double -> Comp -> Sem ()
rightC a = offsetC (a,0)

leftC :: Double -> Comp -> Sem ()
leftC a = offsetC (-a,0)

raiseC :: Double -> Comp -> Sem ()
raiseC a = offsetC (0,a)

lowerMorph :: Double -> Comp -> Sem ()
lowerMorph a = offsetMorph (0,-a) (0,-a)

raiseMorph :: Double -> Comp -> Sem ()
raiseMorph a = offsetMorph (0,a) (0,a)

rightMorph :: Double -> Comp -> Sem ()
rightMorph a = offsetMorph (a,0) (a,0)

leftMorph :: Double -> Comp -> Sem ()
leftMorph a = offsetMorph (-a,0) (-a,0)

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
