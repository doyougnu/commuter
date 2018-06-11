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

setLabelSizeO :: Double -> Custom Obj
setLabelSizeO = set fSize

setLabelSizeM :: Double -> Custom Morph
setLabelSizeM = set mfSize
