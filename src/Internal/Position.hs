module Position where

import Control.Monad        (liftM)
import Control.Lens

import Internal.Types
import Internal.Core

-- | Learning lenses. We tack the morphism, view the mPos field, because it's a
-- maybe we supply a default, if its nothing replace it with a Loc' 0 0, and
-- then scale the x field by the input double
transXL :: Double -> Morph -> Morph
transXL i m = m & mPos %~ non def . x +~ i

-- | Same for Y
transYL :: Double -> Morph -> Morph
transYL i m = m & mPos %~ non def . y +~ i

transL :: Double -> Double -> Morph -> Morph
transL x_ y_ m = m & transYL y_ . transXL x_

-- | Just mutate the from field for the morph
transFrom :: Double -> Double -> Morph -> Morph
transFrom = (flip overLoc_ id .) . updateXY

-- | Just mutate the from field for the morph
transFrom' :: (Loc -> Loc) -> Morph -> Morph
transFrom' = over $ mFrom . oPos

-- | Just mutate the to field for the morph given two doubles
transTo :: Double -> Double -> Morph -> Morph
transTo = (overLoc_ id .) . updateXY

-- | Directly mutate the location of a morph given a unary function
transTo' :: (Loc -> Loc) -> Morph -> Morph
transTo' =  over $ mTo . oPos

-- | set the from field for the morph given two doubles
setFrom :: Double -> Double -> Morph -> Morph
setFrom = (flip overLoc_ id .) . setXY

-- | Given a location set the morph's location to the one provided
setFrom' :: Loc -> Morph -> Morph
setFrom' = set $ mFrom . oPos

-- | set the To field for the morph
setTo :: Double -> Double -> Morph -> Morph
setTo = (overLoc_ id .) . setXY

-- | set the To field for the morph
setTo' :: Loc -> Morph -> Morph
setTo' = set $ mTo . oPos

-- | given two pairs of coordinates apply both to the morph
trans :: (Double, Double) -> (Double, Double) -> Morph -> Morph
trans fcs tcs = overLoc_ (uncurry updateXY fcs) (uncurry updateXY tcs)

-- | the unticked version of trans' is overLoc_
trans' :: (Loc -> Loc) -> (Loc -> Loc) -> Morph -> Morph
trans' = overLoc_

-- | helper function to set the position instead of applying a transformation
setML :: (Double,Double) -> (Double,Double) -> Morph -> Morph
setML fcs tcs = overLoc_ (uncurry setXY fcs) (uncurry setXY tcs)

-- | Set both locations in a Morph given two locations
setML' :: Loc -> Loc -> Morph -> Morph
setML' floc tloc = overLoc_ (const floc) (const tloc)

-- | set the location in the context of the comm monad
setL :: (Double,Double) -> (Double,Double) -> Comm Morph -> Comm Morph
setL = (liftM .) . setML

setPos :: [((Double, Double), (Double, Double))] -> Comm Comp -> Comm Comp
setPos cs (Right xs) = Right $ zipWith (uncurry setML) cs xs
setPos _  a@(Left _) = a
