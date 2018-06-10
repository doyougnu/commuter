module Internal.Position where

import Control.Lens hiding  (under)
import Control.Monad.State  (get, modify, MonadState)
import Control.Monad.Except (MonadError, throwError, catchError)
import Data.Maybe           (isNothing)
import Data.Map             ((!), adjust)

import Internal.Types
import Internal.Core

-- -- | Learning lenses. We tack the morphism, view the mPos field, because it's a
-- -- maybe we supply a default, if its nothing replace it with a Loc' 0 0, and
-- -- then scale the x field by the input double
-- transXL :: Double -> Morph -> Morph
-- transXL i m = m & mPos %~ non def . x +~ i

-- -- | Same for Y
-- transYL :: Double -> Morph -> Morph
-- transYL i m = m & mPos %~ non def . y +~ i

-- transL :: Double -> Double -> Morph -> Morph
-- transL x_ y_ = transYL y_ . transXL x_

-- -- | Just mutate the from field for the morph
-- transFrom :: Double -> Double -> Morph -> Morph
-- transFrom = (flip overLoc_ id .) . updateXY

-- -- | Just mutate the from field for the morph
-- transFrom' :: (Loc -> Loc) -> Morph -> Morph
-- transFrom' = over $ mFrom . oPos

-- -- | Just mutate the to field for the morph given two doubles
-- transTo :: Double -> Double -> Morph -> Morph
-- transTo = (overLoc_ id .) . updateXY

-- -- | Directly mutate the location of a morph given a unary function
-- transTo' :: (Loc -> Loc) -> Morph -> Morph
-- transTo' =  over $ mTo . oPos

-- | set the from field for the morph given two doubles
setFrom :: Double -> Double -> Morph -> Sem ()
setFrom = (flip overLoc_ id .) . setXY

-- | Given a location set the morph's location to the one provided
setFrom' :: Loc -> Morph -> Sem ()
setFrom' = flip overLoc_ id . const

-- | set the To field for the morph
setTo :: Double -> Double -> Morph -> Sem ()
setTo = (overLoc_ id .) . setXY

-- | set the To field for the morph
setTo' :: Loc -> Morph -> Sem ()
setTo' = overLoc_ id . const

-- | given two pairs of coordinates apply both to the morph
trans :: (Double, Double) -> (Double, Double) -> Morph -> Sem ()
trans fcs tcs = overLoc_ (uncurry updateXY fcs) (uncurry updateXY tcs)

-- | the unticked version of trans' is overLoc_
trans' :: (Loc -> Loc) -> (Loc -> Loc) -> Morph -> Sem ()
trans' = overLoc_

-- | apply an offset only to the x coordinate for both objects in a morph
transX :: Double -> Morph -> Sem ()
transX d = overLoc_ f f
  where f = non def . x %~ (+d)

-- | apply an offset only to the y coordinate for both objects in a morph
transY :: Double -> Morph -> Sem ()
transY d = overLoc_ f f
  where f = non def . y %~ (+d)

-- | helper function to set the position instead of applying a transformation
setMLoc :: (Double,Double) -> (Double,Double) -> Morph -> Sem ()
setMLoc fcs tcs = overLoc_ (uncurry setXY fcs) (uncurry setXY tcs)

-- | Set an objects location in the state map regardless of what is there
setOLoc :: (Ord k, MonadState (Map k Obj) m) => Double -> Double -> k -> m ()
setOLoc x_ y_ = modify . adjust (oPos .~ Just (Loc' x_ y_))

mkMphAt :: String ->
  String ->
  String ->
  (Double,Double) ->
  (Double,Double) -> Sem Morph
mkMphAt frm lbl to_ frmCoord toCoord = do m <- mkMph frm lbl to_
                                          setMLoc frmCoord toCoord m
                                          return m

mkObjAt :: String -> (Double,Double) -> Sem String
mkObjAt o a= do o' <- mkObj o
                _ <- uncurry setOLoc a o'
                return o'

-- | Set the position for a composed morphism
setPos :: [((Double, Double), (Double, Double))] -> Comp -> Sem ()
setPos cs xs = sequence_ [ uncurry setMLoc c x_ | c <- cs, x_ <- xs ]

underBy :: Double -> Sem Comp -> Sem Comp -> Sem Comp
underBy d low high = low >>= return . mapM_ (transY d) >> low `merge` high

under :: Sem Comp -> Sem Comp -> Sem Comp
under = underBy 2

onTop :: Sem Comp -> Sem Comp -> Sem Comp
onTop = flip under

underEBy :: Double -> Sem Equ -> Sem Equ -> Sem Equ
underEBy d lows highs = do ls <- lows
                           sequence_ $ mapM_ (transY d) <$> ls
                           lows `mergeE` highs

underE :: Sem Equ -> Sem Equ -> Sem Equ
underE = underEBy 2
