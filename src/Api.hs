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
           , arrows
           , arrowAt
           , arrowsAt
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
           , objectLabels) where

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Prelude hiding ((<>), tri, under,adjust,at,trace,_x,_y,arrow,arrowAt,coords,fc,offset)
import Control.Monad.ListM (zipWithM3,zipWithM5)
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

-- | Given any function that operates on objects, and a pointer to an object,
-- apply that function to the object
copyWith :: (Obj -> Obj) -> String -> Sem String
copyWith f o = do o' <- getObj o
                  let newO = f o'
                  _ <- insertObj newO
                  return (_name newO)

-- | Given an object pointer, copy that object and append a ' to the label
prime :: String -> Sem String
prime o = copyWith (name %~ flip (++) "'") o

-- | create an arrow with the unique type
uniqueArr :: String -> String -> String -> Sem Morph
uniqueArr frm lbl to_ = unique <$> mkMph frm lbl to_

-- | construct an arrow given a from object, a label, and a to object
arrow :: String -> String -> String -> Sem Morph
arrow = mkMph

-- | Construct many arrows given a list of from objects, a list of labels, and a
-- list of to objects
arrows :: [String] -> [String] -> [String] -> Sem Comp
arrows = zipWithM3 mkMph

-- | construct and arrow, l, from an object f, to an object t, where f is at fc,
-- and t is at tc
arrowAt :: String -> String -> String ->
  (Double,Double) -> (Double,Double) -> Sem Morph
arrowAt f l t fc tc = do a <- arrow f l t
                         _ <- setMLoc fc tc a
                         return a

-- | Make several arrows at several locations
arrowsAt :: [String] -> [String] -> [String] ->
  [(Double,Double)] -> [(Double,Double)] -> Sem [Morph]
arrowsAt = zipWithM5 arrowAt

-- | Given a list of labels, and two compositions, try to merge the compositions
-- in any legal way and derive an equivalency between many possible combinations
-- of the compositions
connectWithLabels :: [String] -> Comp -> Comp -> Sem Equ
connectWithLabels ls os us = do newMs' <- newMs
                                let oss = newMs' `merge'` os
                                    uss = newMs' `merge'` us
                                uss `mergeE` oss
  where
    oObjects = objectNamesC os
    uObjects = objectNamesC us
    newMs = zipWithM3 mkMph oObjects ls uObjects

-- | Given a list of labels, and a list of object pointers, construct a
-- composition from the list of objects by intercalating an arrow between every
-- 2 objects in the list
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

-- | generate an infinite list of coordinates in the X direction by a step
-- amount, z
coordsX :: Double -> Double -> Double -> [(Double, Double)]
coordsX xs _ z = zip [xs,(xs+z)..] $ repeat 0

-- | generate an infinite list of coordinates in the Y direction by a step
-- amount, z
coordsY :: Double -> Double -> Double -> [(Double, Double)]
coordsY _ xs z = zip (repeat 0) [xs,(xs+z)..]

-- | Move an arrow by a specified amount
offsetMorph :: (Double,Double) -> (Double,Double) -> Comp -> Sem ()
offsetMorph = transC

-- | lower a morphism by a specified amount
lowerMorph :: Double -> Comp -> Sem ()
lowerMorph a = offsetMorph (0,-a) (0,-a)

-- | raise a morphism by a specified amount
raiseMorph :: Double -> Comp -> Sem ()
raiseMorph a = offsetMorph (0,a) (0,a)

-- | move a morphism right by a specified amount
rightMorph :: Double -> Comp -> Sem ()
rightMorph a = offsetMorph (a,0) (a,0)

-- | move a morphism left by a specified amount
leftMorph :: Double -> Comp -> Sem ()
leftMorph a = offsetMorph (-a,0) (-a,0)

-- | Generate an infinite stream of labels given a function to apply on all of
-- them and a seed value
labelsBy :: (String -> String) -> Char -> [String]
labelsBy f a = f . (:[]) <$> [a..]

-- | Generate a truncated stream of labels that ends at b
labelsBetweenBy :: (String -> String) -> Char -> Char -> [String]
labelsBetweenBy f a b = f . (:[]) <$> [a..b]

-- | generate an infinite stream of lower case labels
labels :: Char -> [String]
labels = labelsBy id

-- | generate a bounded stream of lower case labels
labelsBetween :: Char -> Char -> [String]
labelsBetween = labelsBetweenBy id

-- | Generate object labels that are canonically upper case
objectLabels :: Char -> Char -> [String]
objectLabels = labelsBetweenBy (fmap toUpper)
