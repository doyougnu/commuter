module Custom where

import Control.Lens

import Internal.Types
import Internal.Core

addType :: Type -> Morph -> Morph
addType t m = m & types %~ ((:)t)

unique :: Morph -> Morph
unique = addType Unique

homo :: Morph -> Morph
homo = addType Homo

epi :: Morph -> Morph
epi = addType Epi

bijection :: Morph -> Morph
bijection = addType Bij

customize :: String -> Custom Obj -> Sem ()
customize o f = modify (adjust (customizations %~ (:) f) o)

customizeM :: (Morph -> Morph) -> Custom Morph
customizeM f = mCustomizations %~ (:) f

setLabelSizeO :: String -> Double -> Sem ()
setLabelSizeO s = customize s . set fSize

setObjectLabelSize :: [String] -> [Double] -> Sem ()
setObjectLabelSize = (sequence_ .) . zipWith setLabelSizeO

setFLabelSizeM :: Double -> Custom Morph
setFLabelSizeM = customizeM . set mfSize

setFLabelSizeC :: Double -> Custom Comp
setFLabelSizeC = fmap . setFLabelSizeM

setFLabelSizeE :: Double -> Custom Equ
setFLabelSizeE = fmap . setFLabelSizeC
