{-# LANGUAGE TemplateHaskell #-}
module Internal.Core ( module Control.Lens
                     , module Control.Monad.Except
                     , module Control.Monad
                     , module Control.Monad.State
                     , module Data.List
                     , module Data.Map
                     , y
                     , x
                     , name
                     , oPos
                     , customizations
                     , frozen
                     , fSize
                     , mFrom
                     , mLabel
                     , mTo
                     , types
                     , mfSize
                     , mCustomizations
                     , mkObj
                     , mkMph
                     , mkMph'
                     , coM
                     , coComp
                     , coEqu
                     , bmap
                     , objectNamesM
                     , objectNamesC
                     , objectNamesE
                     , insertObj
                     , getObj
                     , onMorph
                     , setXY
                     , toLoc
                     , wrapDouble
                     , overLoc_
                     , updateXY
                     , overLocO_
                     , domain
                     , range
                     , setDomain'
                     , setRange'
                     , setDomain
                     , setRange
                     , liftToComp
                     , liftToEqu
                     , Composable(..)
                     , Equatable(..)
                     , (|..|)
                     , (|=|)
                     , compM
                     , comp'
                     , comp''
                     , tri
                     , sqr
                     , combinations
                     , combinations_
                     , merge'
                     , merge
                     , mergeE'
                     , mergeE
                     , join
                     , sortE'
                     , sortE
                     , emptySt) where


import Control.Lens hiding  (under,AReview,(#))
import Control.Monad.Except (throwError, catchError)
import Control.Applicative  ((<|>))
import Control.Monad        (liftM, liftM2)
import Control.Monad.State  (modify, get)
import Data.List            (sort,nub,tails)
import Data.Map hiding      (null, (\\))

import Internal.Types
import Debug.Trace (trace)

-- | now we derive lenses for this mess
makeLenses ''Loc'
makeLenses ''Obj
makeLenses ''Morph
-- makeLenses ''Morph2

wrapDouble :: ((Double,Double) -> (Double,Double)) -> Loc -> Loc
wrapDouble _ Nothing = Nothing
wrapDouble f (Just Loc'{..}) = Just $ Loc' a b
  where (a,b) = f (_x,_y)

-- | Given a name create an object with that string as its name
mkObj :: String -> Sem String
mkObj s = do let o = def & name.~ s
             insertObj o
             return s

-- | Smart constructors
mkMph' :: String -> String -> String -> Morph
mkMph' f lbl t = def
                 & mFrom  .~ f
                 & mLabel .~ lbl
                 & mTo    .~ t

-- mkMph2 :: String -> Morph -> Morph2
-- mkMph2 lbl t = def & m2Label .~ lbl
--                    & m2To    .~ t

mkMph :: String -> String -> String -> Sem Morph
mkMph frm lbl to_
  | null frm || null to_ || null lbl = throwError handler
  | otherwise = do os <- get
                   frm' <- if member frm os then return frm else mkObj frm
                   to'  <- if member to_ os then return to_ else mkObj to_
                   let m = mkMph' frm' lbl to'
                   return m
  where
    handler :: ErrMsg
    handler = NoObj $ "Object names cannot be empty: "
              ++ show (mkMph' frm lbl to_)

-- | Reverse the direction of the morphism
coM :: Morph -> Morph
coM m = m & mFrom .~ to_ & mTo .~ frm
  where frm = _mFrom m
        to_ = _mTo m

-- | reverse the direction of the composition
coComp :: Comp -> Comp
coComp = fmap coM

-- | reverse the direction of the equivalence
coEqu :: Equ -> Equ
coEqu = fmap coComp

-- | map on the from and to fields of a morph
bmap :: (String -> String) -> (String -> String) -> Morph -> Morph
bmap f g = (mFrom %~ f) . (mTo %~ g)

-- | get the object names out of a morph
objectNamesM :: Morph -> [String]
objectNamesM m = [_mFrom m, _mTo m]

-- | get the object names out of a composition
objectNamesC :: Comp -> [String]
objectNamesC = nub . concatMap objectNamesM

-- | get the object names out of an equivalence
objectNamesE :: Equ -> [String]
objectNamesE = nub . concatMap objectNamesC

-- | Insert an object into the state
insertObj :: Obj -> Sem ()
insertObj o = modify $ insert (_name o) o

getObj :: String -> Sem Obj
getObj o = do {st <- get; return $ st ! o} `catchError` handler
  where handler _ = throwError . NoObj $ "Object: " ++ o ++ " does not exist!"

-- | Build a Location given two doubles
setXY :: Double -> Double -> Loc -> Loc
setXY x_ y_ = non def %~ (x .~ x_) . (y .~ y_)

toLoc :: Double -> Double -> Loc
toLoc x_ y_ = setXY x_ y_ Nothing

-- | Helper function that will mutate both the mFrom and mTo fields for a Morph
-- given two Location setters. This occurs in a single access traversal
overLoc_ :: (Loc -> Loc) -> (Loc -> Loc) -> Morph -> Sem ()
overLoc_ f g m = modify $ adjust (oPos %~ f) (_mFrom m) . adjust (oPos %~ g) (_mTo m)

-- | Given an x and y coordinate, use non default to wrap the maybe location
-- with a default value. If the Location is Nothing, use the default and
-- translate the x and y fields according to the input coords.
updateXY :: Double -> Double -> Loc -> Loc
updateXY x_ y_ = non def %~ (x +~ x_) . (y +~ y_)

overLocO_ :: (Loc -> Loc) -> String -> Sem ()
overLocO_ f = modify . adjust (oPos %~ f)

-- | apply some function to morphisms on a composition
onMorph :: (Morph -> Morph) -> Comp -> Comp
onMorph = fmap

-- | these are just lenses I don't know how to write
domain :: Comp -> Sem String
domain cs = (return . _mFrom . last $ cs) `catchError` handler
  where handler _ = throwError . NoObj $ "Could not find domain on: " ++ show cs

coDomain :: Comp -> Sem String
coDomain cs = (return . _mTo . head $ cs) `catchError` handler
  where handler _ = throwError . NoObj $ "Could not find range/coDomain on: " ++ show cs

range :: Comp -> Sem String
range = coDomain

setDomain' :: String -> Morph -> Morph
setDomain' = set mFrom

setRange' :: String -> Morph -> Morph
setRange' = set mTo

setDomain :: String -> Comp -> Sem Comp
setDomain _ [] = throwError . NoObj $ "Cannot set domain on empty composition"
setDomain o (e:[]) = return $ setDomain' o e : []
setDomain o (m:ms) = liftM2 (:) (return m) (setDomain o ms)

setRange :: String -> Comp -> Sem Comp
setRange _ [] = throwError . NoObj $ "Cannot set range on empty composition"
setRange o (e:[]) = return $ setRange' o e : []
setRange o (m:ms) = liftM2 (:) (return m) (setRange o ms)

liftToComp :: Morph -> Sem Comp
liftToComp = return . pure

liftToEqu :: Morph -> Sem Equ
liftToEqu = return . pure . pure

-- | smart constructors. take two morphisms and force them to compose by
-- prefering the rhs and setting the lhs domain to the rhs's range
infixr 9 |..|
(|..|) :: Morph -> Morph -> Comp
fs |..| gs = [fs',gs]
  where gRange = _mTo gs
        fs' = setDomain' gRange fs


-- instance Composable Morph Morph (Sem Comp) where (|.|) = compM
-- instance Composable Morph Morph (Sem Equ) where
--   m1 |.| m2 = do compd <- m1 `compM` m2
--                  return . pure $ compd
-- instance Composable (Sem Morph) (Sem Morph) (Sem Equ) where
--   m1 |.| m2 = do m1' <- m1
--                  m2' <- m2
--                  m1' |.| m2'
instance Composable (Sem Morph) (Sem Morph) (Sem Comp) where
  a |.| b = do a' <- a
               b' <- b
               a' `compM` b'

-- instance Composable Morph (Sem Comp) (Sem Comp) where a |.| b = liftToComp a `comp` b
-- instance Composable (Sem Comp) Morph (Sem Comp) where a |.| b = a |.| liftToComp b
-- instance Composable (Sem Morph) (Sem Comp) (Sem Comp) where
--   a |.| b = do a' <- a
--                (liftToComp a') |.| b
-- instance Composable (Sem Comp) (Sem Morph) (Sem Comp) where
--   a |.| b = do b' <- b
--                a |.| (liftToComp b')
instance Composable Comp Comp (Sem Comp) where (|.|)  = comp'
-- instance Composable Comp (Sem Comp) (Sem Comp) where
--   a |.| b = (return a :: Sem Comp) |.| b
-- instance Composable Morph Morph (Sem Comp) where (|.|) = compM

compM :: Morph -> Morph -> Sem Comp
f `compM` g | fD == gR = return [f,g]
            | otherwise = throwError $ MisMatch err
  where gR = _mTo g
        fD = _mFrom f
        err = "The range of " ++ show g
              ++ " does not match the domain of " ++ show f

-- | Smart constructor that will type check the morphisms domain and codomain
-- instance Composable Comp Comp (Sem Comp) where (|.|) = comp'

-- | Compose two compositions with but track the error
comp' :: Comp -> Comp -> Sem Comp
fs `comp'` gs | null result = handler
              | otherwise   = return result
  where
    err = "The range of " ++ show gs ++ " does not match the domain of " ++ show fs
    handler = throwError $ MisMatch err
    result = fs `comp''` gs

-- | compose two compositions with a check for composition
comp'' :: Comp -> Comp -> Comp
[] `comp''` gs = gs
fs `comp''` [] = fs
fs `comp''` gs | range' gs == domain' fs = fs ++ gs
               | otherwise = fs
  where
    range' = _mTo . head
    domain' = _mFrom . last


instance Composable (Sem Comp) (Sem Comp) (Sem Comp) where (|.|) = comp
comp :: Sem Comp -> Sem Comp -> Sem Comp
fs `comp` gs = do f <- fs
                  g <- gs
                  f `comp'` g

-- | smart constructor for :=:, prefers the rhs and sets the lhs's domain and
-- codmain to that of the rhs
infixr 3 |=|
(|=|) :: Sem Comp -> Sem Comp -> Sem Equ
lhs |=| rhs = do rhsDomain <- rhs >>= domain
                 rhsRange <- rhs >>= range
                 lhs' <- lhs >>= setDomain rhsDomain
                 lhs'' <- setRange rhsRange lhs'
                 rhs' <- rhs
                 return $ [lhs'', rhs']

-- | This is Sem Comp for convienience it could easily just be comp -> comp ->
-- comm Equ
instance Equatable (Sem Comp) (Sem Equ) where (|==|) = eq'
instance Equatable (Sem Morph) (Sem Equ) where
  a |==| b = do a' <- a
                b' <- b
                overLoc_ id (fmap (x %~ (+2))) b'
                liftToComp a' |==| liftToComp b'

eq' :: Sem Comp -> Sem Comp -> Sem Equ
lhs `eq'` rhs = do lhsR <- lhs >>= range
                   lhsD <- lhs >>= domain
                   rhsR <- rhs >>= range
                   rhsD <- rhs >>= domain
                   if lhsR == rhsR && lhsD == rhsD
                     then sequence $ [lhs,rhs]
                     else handler
  where
    handler = do l <- lhs
                 r <- rhs
                 let err = "The range or domain of " ++ show l ++
                       " does not match the range or domain of " ++ show r
                 throwError $ MisMatch err

-- | A triangle shape
tri :: (Composable a2 b2 a1, Equatable a1 b1) => a2 -> b2 -> a1 -> b1
tri f g h = (f |.| g) |==| h

-- | a square shape
sqr :: (Composable a2 b2 (Sem Comp), Composable a1 b1 (Sem Comp)) => a1 -> b1 -> a2 -> b2 -> Sem Equ
sqr f g h i = f |.| g |=| h |.| i

-- [[g,f], [i,h]] [[l,g],[k,j]] ==> [[k,j,f],[l,g,f],[l,i,h]]

-- [[g,f], [i,h]] [[k,j],[f,l]] ==> [[g,k,j],[g,f,l],[i,h,l]]

-- | get all possible combinations for a list
combinations :: [a] -> [[a]]
combinations xs = concatMap (flip combinations_ xs) ys
  where
    s = length xs
    ys = [s,s-1..1]

-- | Combinations engine, TODO move to utilities file
combinations_ :: Int -> [a] -> [[a]]
combinations_ 0 _ = [[]]
combinations_ n xs = [ y:ys | y:xs' <- tails xs
                            , ys <- combinations_ (n-1) xs']

-- | return a list of all legal compositions between two compositions
merge' :: Comp -> Comp -> Sem [Comp]
merge' lhs []  = return $ pure lhs
merge' []  rhs = return $ pure rhs
merge' lhs rhs = if null result
                  then throwError . MisMatch $ err
                  else return result
  where
    lhs' :: [Comp]
    lhs' = combinations lhs
    rhs' :: [Comp]
    rhs' = combinations rhs
    result = nub . concat $ [ [ l `comp''` r, r `comp''` l ]
                            | l <- lhs'
                            , r <- rhs'
                            ]
    err = "Could not compose: " ++ show lhs ++ " with " ++ show rhs

-- | attempt to merge a composition with another composition, returning all
-- legal combinations of those compositions
merge :: Sem Comp -> Sem Comp -> Sem [Comp]
merge lhs rhs = do l <- lhs
                   r <- rhs
                   l `merge'` r

mergeE' :: Equ -> Equ -> Sem Equ
mergeE' e   [] = return e
mergeE' []  e  = return e
mergeE' lhs rhs = if null solution
                  then throwError $ NoObj err
                  else return solution
  where range' = _mTo . head
        solution = nub . concat $
                   [ [ l,r ] | l <- lhs , r <- rhs , range' l == range' r]
        err = "Cannot find a shared range between: " ++ show lhs ++ " and :" ++ show rhs

mergeE :: Sem Equ -> Sem Equ -> Sem Equ
mergeE lhs rhs = do l <- lhs
                    r <- rhs
                    l `mergeE'` r

join :: Equ -> Equ -> Sem Equ
join lhs rhs = foldr1 ((<|>)) [ l `merge'` r | l <- lhs, r <- rhs]

sortE' :: Equ -> Equ
sortE' = sort

sortE :: Monad m => m Equ -> m Equ
sortE = liftM sort

emptySt :: PosMap
emptySt = empty
