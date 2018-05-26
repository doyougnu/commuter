{-# LANGUAGE TemplateHaskell #-}
module Internal.Core where

import Control.Lens
import Data.Map       (insert)
import Control.Arrow  (first)
import Internal.Types

-- | now we derive lenses for this mess
makeLenses ''Loc'
makeLenses ''Obj
makeLenses ''Morph
makeLenses ''Morph2
makeLenses ''Comm

mkObj :: String -> Obj
mkObj s = def & name.~ s

mkMph :: String -> Obj -> Morph
mkMph lbl t = def & mLabel .~ lbl
                  & mTo    .~ t

mkMph2 :: String -> Morph -> Morph2
mkMph2 lbl t = def & m2Label .~ lbl
                   & m2To    .~ t

obj :: String -> Comm
obj s = C . first (insert (mkObj s) []) $ unC def
