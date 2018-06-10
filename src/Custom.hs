module Custom where

import Control.Lens

import Internal.Types
import Internal.Core

addCustom :: Type -> Morph -> Morph
addCustom t m = m & types %~ ((:)t)

unique :: Morph -> Morph
unique = addCustom Unique

homo :: Morph -> Morph
homo = addCustom Homo

epi :: Morph -> Morph
epi = addCustom Epi

bijection :: Morph -> Morph
bijection = addCustom Bij
