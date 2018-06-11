module Custom where

import Control.Lens

import Internal.Types
import Internal.Core

import Debug.Trace (trace)

-- | manipulate a morphism to be the input type
addType :: Type -> Morph -> Morph
addType t m = m & types %~ ((:)t)

-- | set a morph type to be unique
unique :: Morph -> Morph
unique = addType Unique

-- | set a morph type to a homomorphism
homo :: Morph -> Morph
homo = addType Homo

-- | set a morph type to a epimorphism
epi :: Morph -> Morph
epi = addType Epi

-- | set a morph type to a bijection
bijection :: Morph -> Morph
bijection = addType Bij

-- | add a customization to an object
customize :: String -> Custom Obj -> Sem ()
customize o f = modify (adjust (customizations %~ (:) f) o)

-- | add a customization to an arrow
customizeM :: (Morph -> Morph) -> Custom Morph
customizeM f = mCustomizations %~ (:) f

-- | set the size of an object label
setLabelSizeO :: String -> Double -> Sem ()
setLabelSizeO s = customize s . set fSize

-- | set the size of many objects' labels at one time
setObjectsLabelSize :: [String] -> [Double] -> Sem ()
setObjectsLabelSize = (sequence_ .) . zipWith setLabelSizeO

-- | set the size of the arrow label
setFLabelSizeM :: Double -> Custom Morph
setFLabelSizeM = customizeM . set mfSize

-- | set the size of every arrow label in an Comp
setFLabelSizeC :: Double -> Custom Comp
setFLabelSizeC = fmap . setFLabelSizeM

-- | set the size of every arrow label in an Equ
setFLabelSizeE :: Double -> Custom Equ
setFLabelSizeE = fmap . setFLabelSizeC

-- | operate on a object's label
swapLabel ::  String -> (String -> String) -> Comp -> Sem Comp
swapLabel obj f cs = do customize obj (name %~ f)
                        modify mapHelper
                        return $ bmap helper helper <$> cs
  where helper a | obj == a = f a
                 | otherwise = a
        mapHelper st = case updateLookupWithKey (\_ _ -> Nothing) obj st of
                         (Nothing, _) -> st
                         (Just e, new) -> insert (f obj) e new

swapLabelE :: String -> (String -> String) -> Equ -> Sem Equ
swapLabelE obj f e =  mapM (swapLabel obj f) e

-- | Convert a string to use latex
mathify :: String -> String
mathify s = "$" ++ s ++ "$"
