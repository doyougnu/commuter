module Internal.Position where

import Control.Lens hiding  (under)
import Control.Monad.State  (modify, MonadState)
import Data.Map             (adjust)

import Internal.Types
import Internal.Core

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
transM :: (Double, Double) -> (Double, Double) -> Morph -> Sem ()
transM fcs tcs = overLoc_ (uncurry updateXY fcs) (uncurry updateXY tcs)

-- | given two pairs of coordinates apply both to the morph
transC :: (Double, Double) -> (Double, Double) -> Comp -> Sem ()
transC = (mapM_ .) . transM

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

offset :: (Double,Double) -> String -> Sem ()
offset = uncurry transO

lower :: Double -> String -> Sem ()
lower a = offset (0,-a)

right :: Double -> String -> Sem ()
right a = offset (a,0)

left :: Double -> String -> Sem ()
left a = offset (-a,0)

raise :: Double -> String -> Sem ()
raise a = offset (0,a)

offsetC :: (Double,Double) -> Comp -> Sem ()
offsetC a = mapM_ (offset a)  . objectNamesC

lowerC :: Double -> Comp -> Sem ()
lowerC a = offsetC (0,-a)

rightC :: Double -> Comp -> Sem ()
rightC a = offsetC (a,0)

leftC :: Double -> Comp -> Sem ()
leftC a = offsetC (-a,0)

raiseC :: Double -> Comp -> Sem ()
raiseC a = offsetC (0,a)

offsetE :: (Double,Double) -> Equ -> Sem ()
offsetE = mapM_ . offsetC

lowerE :: Double -> Equ -> Sem ()
lowerE a = offsetE (0,-a)

rightE :: Double -> Equ -> Sem ()
rightE a = offsetE (a,0)

leftE :: Double -> Equ -> Sem ()
leftE a = offsetE (-a,0)

raiseE :: Double -> Equ -> Sem ()
raiseE a = offsetE (0,a)

-- | helper function to set the position instead of applying a transformation
setMLoc :: (Double,Double) -> (Double,Double) -> Morph -> Sem ()
setMLoc fcs tcs = overLoc_ (uncurry setXY fcs) (uncurry setXY tcs)

-- | Set an objects location in the state map regardless of what is there
setOLoc :: (Ord k, MonadState (Map k Obj) m) => Double -> Double -> k -> m ()
setOLoc x_ y_ = modify . adjust (oPos .~ Just (Loc' x_ y_))

transO :: Double -> Double -> String -> Sem ()
transO x_ y_ = overLocO_ (updateXY x_ y_)

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
underBy d low high = low >>= return . mapM_ (transY d) >> low |.| high

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
