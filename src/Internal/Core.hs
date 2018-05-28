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

-- | Given an x and y coordinate, use non default to wrap the maybe location
-- with a default value. If the Location is Nothing, use the default and
-- translate the x and y fields according to the input coords.
updateXY :: Double -> Double -> Loc -> Loc
updateXY x_ y_ = non def %~ (x +~ x_) . (y +~ y_)

-- | Helper function that will mutate both the mFrom and mTo fields for a Morph
-- given two Location setters
trans_ :: (Loc -> Loc) -> (Loc -> Loc) -> Morph' -> Morph'
trans_ f g = (mFrom . oPos %~ f) . (mTo . oPos %~ g)

-- | Just mutate the from field for the morph
transFrom :: Double -> Double -> Morph' -> Morph'
transFrom = (flip trans_ id .) . updateXY

-- | Just mutate the to field for the morph
transTo :: Double -> Double -> Morph' -> Morph'
transTo = (trans_ id .) . updateXY

-- | given two pairs of coordinates apply both to the morph
trans :: (Double, Double) -> (Double, Double) -> Morph' -> Morph'
trans fcs tcs = trans_ (uncurry updateXY fcs) (uncurry updateXY tcs)

-- | TODO repeat the trans mutations but for setting instead of updating. Then
-- define the composition operation to ensure that the lcoation of the domains
-- and codomains mathc. Then do the same with equivalence. that is on the domain
-- of the LHS must match the domain on the RHS and the range on the RHS must
-- match the range on the LHS
-- tri :: Morph' -> Morph' -> Morph' -> Equation
-- tri f g h = (E $ trans (0,0) (2,0) f) :.: (E $ trans (2,0) (2, negate 2) g) :=: (E $ transL 2 (-2) h)

-- sqr :: Morph' -> Morph' -> Morph' -> Morph' -> Equation
-- sqr f g h i = (E $ transL 0 0 f) :.: (E $ transL 2 0 g) :=: (E h) :.: (E i)
