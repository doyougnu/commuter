{-# LANGUAGE TemplateHaskell #-}
module Internal.Core where

import Control.Lens
import Control.Monad.Except (catchError)
import Control.Monad        (liftM,liftM2)
import Control.Monad.State  (modify)
import Data.Semigroup       ((<>))
import Data.List            (sort,nub)
import Data.Map

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
mkMph' :: String -> String -> String -> Morph
mkMph' f lbl t = def
                & mFrom  .~ f
                & mLabel .~ lbl
                & mTo    .~ t

mkMph2 :: String -> Morph -> Morph2
mkMph2 lbl t = def & m2Label .~ lbl
                   & m2To    .~ t

-- | get the object names out of a morph
objectNamesM :: Morph -> [String]
objectNamesM m = [_mFrom m, _mTo m]

-- | get the object names out of a composition
objectNamesC :: Comp -> [String]
objectNamesC = concatMap objectNamesM

-- | get the object names out of an equivalence
objectNamesE :: Equ -> [String]
objectNamesE = concatMap objectNamesC

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

-- | these are just lenses I don't know how to write
domain :: Comp -> Comm String
domain cs = (return . _mFrom . last $ cs) `catchError` handler
  where handler _ = Left . NoObj $ "Could not find domain on: " ++ show cs

coDomain :: Comp -> Comm String
coDomain cs = (return . _mTo . head $ cs) `catchError` handler
  where handler _ = Left . NoObj $ "Could not find range/coDomain on: " ++ show cs

range :: Comp -> Comm String
range = coDomain

setDomain' :: String -> Morph -> Morph
setDomain' = set mFrom

setRange' :: String -> Morph -> Morph
setRange' = set mTo

setDomain :: String -> Comp -> Comm Comp
setDomain _ [] = Left . NoObj $ "Cannot set domain on empty composition"
setDomain o (e:[]) = return $ setDomain' o e : []
setDomain o (m:ms) = liftM2 (:) (return m) (setDomain o ms)

setRange :: String -> Comp -> Comm Comp
setRange _ [] = Left . NoObj $ "Cannot set range on empty composition"
setRange o (e:[]) = return $ setRange' o e : []
setRange o (m:ms) = liftM2 (:) (return m) (setRange o ms)

liftToComp :: Morph -> Comm Comp
liftToComp = return . pure

mkMph :: String -> String -> String -> Comm Comp
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

-- | A triangle shape
tri :: Comm Comp -> Comm Comp -> Comm Comp -> Comm Equ
tri f g h = f |.| g |==| h

-- | a square shape
sqr :: Comm Comp -> Comm Comp -> Comm Comp -> Comm Comp -> Comm Equ
sqr f g h i = f |.| g |=| h |.| i

-- [[g,f], [i,h]] [[l,g],[k,j]] ==> [[k,j,f],[l,g,f],[l,i,h]]

-- [[g,f], [i,h]] [[k,j],[f,l]] ==> [[g,k,j],[g,f,l],[i,h,l]]

merge' :: Comp -> Comp -> Comm Comp
merge' lhs []  = return lhs
merge' []  rhs = return rhs
merge' lhs rhs = foldr1 ((<>)) $ left ++ right
  where left = do l <- lhs
                  let l' = liftToComp l
                  return $ (l' |.| (return rhs)) <> (return rhs |.| l')
        right = do r <- rhs
                   let r' = liftToComp r
                   return $ (r' |.| (return lhs)) <> (return lhs |.| r')

merge :: Comm Comp -> Comm Comp -> Comm Comp
merge lhs rhs = do l <- lhs
                   r <- rhs
                   l `merge'` r

mergeE' :: Equ -> Equ -> Equ
mergeE' lhs rhs = nub $ concat [ [l,r ] | l <- lhs, r <- rhs, range l == range r]

mergeE :: Monad m => m Equ -> m Equ -> m Equ
mergeE = liftM2 mergeE'

sortE' :: Equ -> Equ
sortE' = sort

sortE :: Monad m => m Equ -> m Equ
sortE = liftM sort

emptySt :: PosMap
emptySt = empty

debugO' :: Obj -> String
debugO' Obj{..} = spacer ++ _name ++ ": \n  " ++ spacer ++ show _frozen
                  ++ spacer ++ show _fSize ++ spacer
  where spacer = "  \n  "

debugM' :: Morph -> String
debugM' Morph{..} = spacer ++ _mLabel ++ ": \n  " ++ _mFrom  ++ spacer
                    ++ _mTo  ++ spacer ++ show _types ++ spacer ++ show _mfsize
                    ++ spacer
  where spacer = " \n  "

debugC' :: Comp -> String
debugC' = Prelude.foldr (++) "" . fmap debugM'

debugE' :: Equ -> String
debugE' = Prelude.foldr (++) "" . fmap debugC'

debugO :: Comm Obj -> String
debugO (Right a) = debugO' a
debugO (Left err) = show err

debugM :: Comm Morph -> String
debugM (Right a) = debugM' a
debugM (Left err) = show err

debugC :: Comm Comp -> String
debugC (Right a) = debugC' a
debugC (Left err) = show err

debugE :: Comm Equ -> String
debugE (Right a) = debugE' a
debugE (Left err) = show err
