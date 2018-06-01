{-# LANGUAGE TemplateHaskell #-}
module Internal.Core where

import Control.Lens

import Internal.Types

-- | now we derive lenses for this mess
makeLenses ''Loc'
makeLenses ''Obj
makeLenses ''Morph'
makeLenses ''Morph2
makeLenses ''Morph''

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
updateXY' :: Double -> Double -> Loc -> Loc
updateXY' x_ y_ = non def %~ (x +~ x_) . (y +~ y_)

-- | Build a Location given two doubles
setXY' :: Double -> Double -> Loc -> Loc
setXY' x_ y_ = non def %~ (x .~ x_) . (y .~ y_)

-- | Helper function that will mutate both the mFrom and mTo fields for a Morph
-- given two Location setters. This occurs in a single access traversal
overLoc_ :: (Loc -> Loc) -> (Loc -> Loc) -> Morph' -> Morph'
overLoc_ f g = (mFrom . oPos %~ f) . (mTo . oPos %~ g)

-- | Just mutate the from field for the morph
transFrom' :: Double -> Double -> Morph' -> Morph'
transFrom' = (flip overLoc_ id .) . updateXY'

-- | Just mutate the from field for the morph
transFrom :: (Loc -> Loc) -> Morph' -> Morph'
transFrom = over $ mFrom . oPos

-- | Just mutate the to field for the morph given two doubles
transTo' :: Double -> Double -> Morph' -> Morph'
transTo' = (overLoc_ id .) . updateXY'

-- | Directly mutate the location of a morph given a unary function
transTo :: (Loc -> Loc) -> Morph' -> Morph'
transTo =  over $ mTo . oPos

-- | given two pairs of coordinates apply both to the morph
trans' :: (Double, Double) -> (Double, Double) -> Morph' -> Morph'
trans' fcs tcs = overLoc_ (uncurry updateXY' fcs) (uncurry updateXY' tcs)

-- | the unticked version of trans' is overLoc_
trans :: (Loc -> Loc) -> (Loc -> Loc) -> Morph' -> Morph'
trans = overLoc_

-- | set the from field for the morph given two doubles
setFrom' :: Double -> Double -> Morph' -> Morph'
setFrom' = (flip overLoc_ id .) . setXY'

-- | Given a location set the morph's location to the one provided
setFrom :: Loc -> Morph' -> Morph'
setFrom = set $ mFrom . oPos

-- | set the To field for the morph
setTo' :: Double -> Double -> Morph' -> Morph'
setTo' = (overLoc_ id .) . setXY'

-- | set the To field for the morph
setTo :: Loc -> Morph' -> Morph'
setTo = set $ mTo . oPos

-- | helper function to set the position instead of applying a transformation
setL' :: (Double,Double) -> (Double,Double) -> Morph' -> Morph'
setL' fcs tcs = overLoc_ (uncurry setXY' fcs) (uncurry setXY' tcs)

-- | Set both locations in a Morph given two locations
setL :: Loc -> Loc -> Morph' -> Morph'
setL floc tloc = overLoc_ (const floc) (const tloc)

-- | these are just lenses I don't know how to write
domain :: Morph -> Obj
domain (M m) = m ^. mFrom
domain (_ :.: ns) = domain ns
domain (_ :=: ns) = domain ns

coDomain :: Morph -> Obj
coDomain (M m) = m ^. mTo
coDomain (ns :.: _) = coDomain ns
coDomain (ns :=: _) = coDomain ns

range :: Morph -> Obj
range = coDomain

setDomain :: Obj -> Morph ->  Morph
setDomain o (M m) = M $ m & mFrom .~ o
setDomain o (ms :.: ns) = ms :.: setDomain o ns
setDomain o (ms :=: ns) = setDomain o ms :=: setDomain o ns

setRange :: Obj -> Morph -> Morph
setRange o (M m) = M $ m & mTo .~ o
setRange o (ms :.: ns) = setRange o ms :.: ns
setRange o (ms :=: ns) = setRange o ms :=: setRange o ns

-- | smart constructors. take two morphisms and force them to compose by prefering the rhs and setting the lhs domain to the rhs's range
infixr 9 |..|
(|..|) :: Morph -> Morph -> Morph
fs |..| gs = setDomain gRange fs :.: gs
  where gRange  = range gs

-- | Smart constructor that will type check the morphisms domain and codomain
infixr 9 |.|
(|.|) :: Morph -> Morph -> Either ErrMsg Morph
fs |.| gs | range gs == domain fs = Right $ fs :.: gs
          | otherwise = Left $ MisMatch err
  where err = "The range of " ++ gs ^. mLabel ++ " does not match the domain of " ++ fs ^. mLabel


-- | smart constructor for :=:, prefers the rhs and sets the lhs's domain and
-- codmain to that of the rhs
(|=|) :: Morph -> Morph -> Morph
lhs |=| rhs = (setRange rhsRange $ setDomain rhsDomain lhs) :=: rhs
  where rhsDomain = domain rhs
        rhsRange = range rhs
infixr 3 |=|
-- | given two pairs of coordinates set both to the morph

-- | TODO repeat the trans mutations but for setting instead of updating. Then
-- define the composition operation to ensure that the lcoation of the domains
-- and codomains mathc. Then do the same with equivalence. that is on the domain
-- of the LHS must match the domain on the RHS and the range on the RHS must
-- match the range on the LHS

tri :: Morph -> Morph -> Morph -> Morph
tri f g h =  (f |.| g) |=| h

-- sqr :: Morph' -> Morph' -> Morph' -> Morph' -> Equation
-- sqr f g h i = (E $ transL 0 0 f) :.: (E $ transL 2 0 g) :=: (E h) :.: (E i)
