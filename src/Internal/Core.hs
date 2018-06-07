{-# LANGUAGE TemplateHaskell #-}
module Internal.Core where

import Control.Lens
import Control.Monad.Except (catchError)
import Control.Monad        (liftM,liftM2)
import Data.Semigroup       ((<>))
import Data.List            (nub, sort)

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

joinE' :: Equ -> Equ -> Equ
joinE' lhs rhs = nub $ concat [ [l,r ]| l <- lhs, r <- rhs, range l == range r]

joinE :: Monad m => m Equ -> m Equ -> m Equ
joinE = liftM2 joinE'

sortE' :: Equ -> Equ
sortE' = sort

sortE :: Monad m => m Equ -> m Equ
sortE = liftM sort
