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
transXL :: Double -> Morph' -> Morph'
transXL i m = m & mPos %~ non def . x +~ i

-- | Same for Y
transYL :: Double -> Morph' -> Morph'
transYL i m = m & mPos %~ non def . y +~ i

transL :: Double -> Double -> Morph' -> Morph'
transL x_ y_ m = m & transYL y_ . transXL x_

-- | Lenses produce some scary ass types. Given some double, and some selector
-- into the Morph' and a selector into an Obj, mutate the double at the location
-- by adding i
updateXY :: Double -> Double -> Loc -> Loc
updateXY x_ y_ = non def %~ (x +~ x_) . (y +~ y_)

trans_ :: (Loc -> Loc) -> (Loc -> Loc) -> Morph' -> Morph'
trans_ f g = (mFrom . oPos %~ f) . (mTo . oPos %~ g)

transFrom :: Double -> Double -> Morph' -> Morph'
transFrom = (flip trans_ id .) . updateXY

transTo :: Double -> Double -> Morph' -> Morph'
transTo = (trans_ id .) . updateXY

tri :: Morph' -> Morph' -> Morph' -> Equation
tri f g h = (E $ transL 0 0 f) :.: (E $ transL 0 0 g) :=: (E $ transL 2 (-2) h)

sqr :: Morph' -> Morph' -> Morph' -> Morph' -> Equation
sqr f g h i = (E $ transL 0 0 f) :.: (E $ transL 2 0 g) :=: (E h) :.: (E i)
