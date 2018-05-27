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

-- | Learning lenses. We tack the morphism, view the mPos field, because it's a
-- maybe we supply a default, if its nothing replace it with a Loc' 0 0, and
-- then scale the x field by the input double
transX :: Double -> Morph' -> Morph'
transX i m = m & mPos %~ non (Loc' 0 0) . x +~ i

-- | Same for Y
transY :: Double -> Morph' -> Morph'
transY i m = m & mPos %~ non (Loc' 0 0) . y +~ i

trans :: Double -> Double -> Morph' -> Morph'
trans x_ y_ m = m & transY y_ . transX x_

tri :: Morph' -> Morph' -> Morph' -> Equation
tri f g h = (E $ trans 0 0 f) :.: (E $ trans 2 0 g) :=: (E $ trans 2 (-2) h)

sqr :: Morph' -> Morph' -> Morph' -> Morph' -> Equation
sqr f g h i = (E f) :.: (E g) :=: (E h) :.: (E i)
