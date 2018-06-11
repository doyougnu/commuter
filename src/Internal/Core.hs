{-# LANGUAGE TemplateHaskell #-}
module Internal.Core where

import Control.Lens
import Control.Monad.Except (throwError, runExceptT, catchError)
import Control.Monad        (liftM, liftM2)
import Control.Monad.State  (evalState, modify,get)
import Control.Applicative  ((<|>))
import Data.List            (sort,nub,tails)
import Data.Map hiding      (null)

import Internal.Types


-- | now we derive lenses for this mess
makeLenses ''Loc'
makeLenses ''Obj
makeLenses ''Morph
makeLenses ''Morph2

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
bmap :: (String -> a) -> (String -> a) -> Morph -> [a]
bmap f g m = [f (_mFrom m), g (_mTo m)]

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

comp' :: Comp -> Comp -> Sem Comp
fs `comp'` gs = do rngGs <- range gs
                   dmnFs <- domain fs
                   if rngGs == dmnFs
                     then return $ fs ++ gs
                     else handler
  where
    err = "The range of " ++ show gs ++ " does not match the domain of " ++ show fs
    handler = throwError $ MisMatch err

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
fs `comp` gs = do rngGs <- gs >>= range
                  dmnFs <- fs >>= domain
                  if rngGs == dmnFs
                    then (++) <$> fs <*> gs >>= return . nub
                    else handler
  where handler = do f <- fs
                     g <- gs
                     let err = "The range of " ++ show g ++ " does not match the domain of " ++ show f
                     throwError $ MisMatch err

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
-- sqr :: Sem Comp -> Sem Comp -> Sem Comp -> Sem Comp -> Sem Equ
-- sqr :: (Composable b (Sem Comp), Composable a (Sem Comp)) =>
--   a -> a -> b -> b -> Sem Equ
sqr :: (Composable a2 b2 (Sem Comp), Composable a1 b1 (Sem Comp)) => a1 -> b1 -> a2 -> b2 -> Sem Equ
sqr f g h i = f |.| g |=| h |.| i

-- [[g,f], [i,h]] [[l,g],[k,j]] ==> [[k,j,f],[l,g,f],[l,i,h]]

-- [[g,f], [i,h]] [[k,j],[f,l]] ==> [[g,k,j],[g,f,l],[i,h,l]]

combinations :: [a] -> [[a]]
combinations xs = concatMap (flip combinations_ xs) ys
  where
    s = length xs
    ys = [s,s-1..1]

combinations_ :: Int -> [a] -> [[a]]
combinations_ 0 _ = [[]]
combinations_ n xs = [ y:ys | y:xs' <- tails xs
                            , ys <- combinations_ (n-1) xs']

-- | return a list of all legal compositions between two compositions
merge'' :: Comp -> Comp -> Sem [Comp]
merge'' lhs []  = return $ pure lhs
merge'' []  rhs = return $ pure rhs
merge'' lhs rhs = if null result
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

merge' :: Comp -> Comp -> Sem Comp
merge' lhs []  = return lhs
merge' []  rhs = return rhs
merge' lhs rhs = foldr1 ((<|>)) $ left ++ right
  where left = do l <- lhs
                  let l' = [l]
                  return $ (l' |.| rhs) <|> (rhs |.| l')
        right = do r <- rhs
                   let r' = [r]
                   return $ (r' |.| lhs) <|> (lhs |.| r')

merge :: Sem Comp -> Sem Comp -> Sem Comp
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

debugO :: Sem Obj -> String
debugO = debugMaker go
  where
    go (Right a) = debugO' a
    go (Left err) = show err

debugMaker :: (Either ErrMsg a -> String) -> Sem a -> String -- fmap?
debugMaker f = f . flip evalState emptySt . runExceptT

debugM :: Sem Morph -> String
debugM = debugMaker go
  where
    go (Right a) = debugM' a
    go (Left err) = show err

debugC :: Sem Comp -> String
debugC = debugMaker go
  where
    go (Right a) = debugC' a
    go (Left err) = show err

debugE :: Sem Equ -> String
debugE = debugMaker go
  where
    go (Right a) = debugE' a
    go (Left err) = show err

customize :: String -> Custom Obj -> Sem ()
customize o f = modify (adjust (customizations %~ (:) f) o)
