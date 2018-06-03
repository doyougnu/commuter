{-# LANGUAGE TemplateHaskell #-}
module Internal.Core where

import Control.Lens
import Control.Monad.Except (catchError)
import Control.Monad        (liftM2, guard)
import Data.Semigroup       ((<>))
import Data.List            (intersect, intersectBy, nub, unionBy)

import Internal.Types


-- | now we derive lenses for this mess
makeLenses ''Loc'
makeLenses ''Obj
makeLenses ''Morph
makeLenses ''Morph2

-- | Given a name create an object with that string as its name
mkObj :: String -> Obj
mkObj s = def & name.~ s

-- | Smart constructors
mkMph' :: Obj -> String -> Obj -> Morph
mkMph' f lbl t = def
                & mFrom  .~ f
                & mLabel .~ lbl
                & mTo    .~ t

mkMph2 :: String -> Morph -> Morph2
mkMph2 lbl t = def & m2Label .~ lbl
                   & m2To    .~ t

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
overLoc_ :: (Loc -> Loc) -> (Loc -> Loc) -> Morph -> Morph
overLoc_ f g = (mFrom . oPos %~ f) . (mTo . oPos %~ g)

-- | Just mutate the from field for the morph
transFrom' :: Double -> Double -> Morph -> Morph
transFrom' = (flip overLoc_ id .) . updateXY'

-- | Just mutate the from field for the morph
transFrom :: (Loc -> Loc) -> Morph -> Morph
transFrom = over $ mFrom . oPos

-- | Just mutate the to field for the morph given two doubles
transTo' :: Double -> Double -> Morph -> Morph
transTo' = (overLoc_ id .) . updateXY'

-- | Directly mutate the location of a morph given a unary function
transTo :: (Loc -> Loc) -> Morph -> Morph
transTo =  over $ mTo . oPos

-- | given two pairs of coordinates apply both to the morph
trans' :: (Double, Double) -> (Double, Double) -> Morph -> Morph
trans' fcs tcs = overLoc_ (uncurry updateXY' fcs) (uncurry updateXY' tcs)

-- | the unticked version of trans' is overLoc_
trans :: (Loc -> Loc) -> (Loc -> Loc) -> Morph -> Morph
trans = overLoc_

-- | set the from field for the morph given two doubles
setFrom' :: Double -> Double -> Morph -> Morph
setFrom' = (flip overLoc_ id .) . setXY'

-- | Given a location set the morph's location to the one provided
setFrom :: Loc -> Morph -> Morph
setFrom = set $ mFrom . oPos

-- | set the To field for the morph
setTo' :: Double -> Double -> Morph -> Morph
setTo' = (overLoc_ id .) . setXY'

-- | set the To field for the morph
setTo :: Loc -> Morph -> Morph
setTo = set $ mTo . oPos

-- | helper function to set the position instead of applying a transformation
setL' :: (Double,Double) -> (Double,Double) -> Morph -> Morph
setL' fcs tcs = overLoc_ (uncurry setXY' fcs) (uncurry setXY' tcs)

-- | Set both locations in a Morph given two locations
setL :: Loc -> Loc -> Morph -> Morph
setL floc tloc = overLoc_ (const floc) (const tloc)

-- | these are just lenses I don't know how to write
domain :: Comp -> Comm Obj
domain cs = (return . _mFrom . last $ cs) `catchError` handler
  where handler _ = Left . NoObj $ "Could not find domain on: " ++ show cs

coDomain :: Comp -> Comm Obj
coDomain cs = (return . _mTo . head $ cs) `catchError` handler
  where handler _ = Left . NoObj $ "Could not find range/coDomain on: " ++ show cs

range :: Comp -> Comm Obj
range = coDomain

setDomain' :: Obj -> Morph -> Morph
setDomain' = set mFrom

setRange' :: Obj -> Morph -> Morph
setRange' = set mTo

setDomain :: Obj -> Comp -> Comm Comp
setDomain _ [] = Left . NoObj $ "Cannot set domain on empty composition"
setDomain o (e:[]) = return $ setDomain' o e : []
setDomain o (m:ms) = liftM2 (:) (return m) (setDomain o ms)

setRange :: Obj -> Comp -> Comm Comp
setRange _ [] = Left . NoObj $ "Cannot set range on empty composition"
setRange o (e:[]) = return $ setRange' o e : []
setRange o (m:ms) = liftM2 (:) (return m) (setRange o ms)

liftToComp :: Morph -> Comm Comp
liftToComp = return . pure

mkMph :: Obj -> String -> Obj -> Comm Comp
mkMph = ((liftToComp. ) .) . mkMph'

liftToEqu :: Morph -> Comm Equ
liftToEqu = return . pure . pure


-- | smart constructors. take two morphisms and force them to compose by prefering the rhs and setting the lhs domain to the rhs's range
infixr 9 |...|
(|...|) :: Morph -> Morph -> Comp
fs |...| gs = [fs',gs]
  where gRange = _mTo gs
        fs' = setDomain' gRange fs

infixr 9 |..|
(|..|) :: Morph -> Morph -> Comm Comp
f |..| g | fD == gR = Right [f,g]
         | otherwise = Left $ MisMatch err
  where gR = _mTo g
        fD = _mFrom f
        err = "The range of " ++ show g
              ++ " does not match the domain of " ++ show f


-- | Smart constructor that will type check the morphisms domain and codomain
infixr 9 |.|
(|.|) :: Comm Comp -> Comm Comp -> Comm Comp
fs |.| gs = do rngGs <- gs >>= range
               dmnFs <- fs >>= domain
               if rngGs == dmnFs
                  then (++) <$> fs <*> gs
                  else Left $ MisMatch err
  where err = "The range of " ++ show gs
              ++ " does not match the domain of " ++ show fs


-- | smart constructor for :=:, prefers the rhs and sets the lhs's domain and
-- codmain to that of the rhs
infixr 3 |=|
(|=|) :: Comm Comp -> Comm Comp -> Comm Equ
lhs |=| rhs = do rhsDomain <- rhs >>= domain
                 rhsRange <- rhs >>= range
                 lhs' <- lhs >>= setDomain rhsDomain
                 lhs'' <- setRange rhsRange lhs'
                 rhs' <- rhs
                 return $ [lhs'', rhs']

-- | This is Comm Comp for convienience it could easily just be comp -> comp ->
-- comm Equ
infixr 3 |==|
(|==|) :: Comm Comp -> Comm Comp -> Comm Equ
lhs |==| rhs = do lhsR <- lhs >>= range
                  lhsD <- lhs >>= domain
                  rhsR <- rhs >>= range
                  rhsD <- rhs >>= domain
                  if lhsR == rhsR && lhsD == rhsD
                     then sequence $ [lhs,rhs]
                     else Left $ MisMatch err
  where
    err = "The range or domain of " ++ show lhs
      ++ " does not match the range or domain of " ++ show rhs

-- | given two pairs of coordinates set both to the morph

-- | TODO repeat the trans mutations but for setting instead of updating. Then
-- define the composition operation to ensure that the lcoation of the domains
-- and codomains mathc. Then do the same with equivalence. that is on the domain
-- of the LHS must match the domain on the RHS and the range on the RHS must
-- match the range on the LHS

tri :: Comm Comp -> Comm Comp -> Comm Comp -> Comm Equ
tri f g h = f |.| g |==| h


sqr :: Comm Comp -> Comm Comp -> Comm Comp -> Comm Comp -> Comm Equ
sqr f g h i = f |.| g |=| h |.| i

-- [[g,f], [i,h]] [[l,g],[k,j]] ==> [[k,j,f],[l,g,f],[l,i,h]]

-- [[g,f], [i,h]] [[k,j],[f,l]] ==> [[g,k,j],[g,f,l],[i,h,l]]

join' :: Comp -> Comp -> Comm Comp
join' lhs []  = return lhs
join' []  rhs = return rhs
join' lhs rhs = foldr ((<>)) (Left $ MisMatch "") $ left ++ right
  where left = do l <- lhs
                  let l' = liftToComp l
                  return $ (l' |.| (return rhs)) <> (return rhs |.| l')
        right = do r <- rhs
                   let r' = liftToComp r
                   return $ (r' |.| (return lhs)) <> (return lhs |.| r')

joinC :: Comm Comp -> Comm Comp -> Comm Comp
joinC lhs rhs = do l <- lhs
                   r <- rhs
                   l `join'` r

join'' lhs rhs = nub $ concat [ [l,r ]| l <- lhs, r <- rhs, range l == range r]

joinE = liftM2 join''

-- | TODO convert to lens at some point
replace :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
replace prd f as = do a <- as
                      if prd a
                        then return $ f a
                        else return a

-- -- labels :: Morph -> [String]
-- -- labels (M a) = pure $ a ^. mLabel
-- -- labels (ns :.: ms) = labels ns ++ labels ms
-- -- labels (ns :=: ms) = labels ns ++ labels ms

-- -- elem' :: Morph -> String -> Bool
-- -- elem' (M a) str = str == a ^. mLabel
-- -- elem' (ns :.: ms) str = elem' ns str && elem' ms str
-- -- elem' (ns :=: ms) str = elem' ns str && elem' ms str


-- -- -- lhsBy :: Morph -> (Morph -> Bool) -> Maybe Morph
-- -- -- lhsBy a@(M x) f acc
-- -- --   | f a == True = Just acc
-- -- --   | otherwise = Nothing
-- -- -- lhsBy (ms :.: ns) f acc =
-- -- -- lhsBy (ms :=: ns) f

-- -- -- join :: Morph -> Morph -> Morph
-- -- -- join m1 m2 = go l
-- -- --   where (l:ls) = L.intersect (labels m1) (labels m2)
