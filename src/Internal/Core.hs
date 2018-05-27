{-# LANGUAGE TemplateHaskell #-}
module Internal.Core where

import Control.Lens

import Internal.Types

-- | now we derive lenses for this mess
makeLenses ''Loc'
makeLenses ''Obj
makeLenses ''Morph'
makeLenses ''Morph2

-- | Given a name create an object with that string as its name
mkObj :: String -> Obj
mkObj s = def & name.~ s

-- | Smart constructors
mkMph :: Obj -> String -> Obj -> Morph'
mkMph f lbl t = def
                & mFrom  .~ f
                & mLabel .~ lbl
                & mTo    .~ t

mkMph2 :: String -> Morph' -> Morph2
mkMph2 lbl t = def & m2Label .~ lbl
                   & m2To    .~ t

tri :: Morph' -> Morph' -> Morph' -> Equation
tri f g h = (E f) :.: (E g) :=: (E h)

sqr :: Morph' -> Morph' -> Morph' -> Morph' -> Equation
sqr f g h i = (E f) :.: (E g) :=: (E h) :.: (E i)
