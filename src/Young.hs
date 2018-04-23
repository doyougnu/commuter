-- | Module for Thursday week 3, showing an example shallow embedding
module Young where

import Data.Set                  (Set, member, fromList)

-------------------------------- Try 1 -----------------------------------------
-- | A naive first step
data Prims a = Obj1 String a
             | Morph1 String (Prims a) (Prims a)

-- This is just an arrow arr a b -> arr b c -> arr a c, without the computation
comp :: Prims a -> Prims a -> Prims a
comp (Obj1 name a) = undefined -- ??? Two domains?

-------------------------------- Try 2 -----------------------------------------
-- | Primitive types, take an a and a b, the a shall be used for identification
-- with b being the concrete data type
-- These are just for a 1-category
data Obj2 a = Obj2 String a                        -- ^ an Object
            deriving (Show,Eq,Ord)

data Morph2 a b = Morph2 String (Obj2 a) (Obj2 b)  -- ^ a general morphism
                deriving (Show,Eq,Ord)

-- | A small category is a category s.t. all objects are Sets and a collection
-- of all morphisms is also a set (the hom-set)
type Hom2 a b = Set (Morph2 a b)

--notice that this diagram only allows on type of Objs
type Diagram a = (Set (Obj2 a), Set (Morph2 a a))

-- this means that a functor goes between diagrams
functor :: (Ord a, Ord b) =>
  Obj2 a -> Obj2 b -> Diagram a -> Diagram b -> Morph2 a b -- But where should
                                                           -- this morphism
                                                           -- live? In Diagram
                                                           -- a? In Diagram b?
                                                           -- In a diagram a b?
                                                           -- The type won't
                                                           -- allow that
functor a@(Obj2 aName _) b@(Obj2 bName _) (dAOs, dAMs) (dBOs, dBMs)
  | a `member` dAOs && b `member` dBOs = Morph2 ("F " ++ aName ++ " " ++ bName) a b

-- notice that this category only allows morphisms  between the same types
type SmallCategory2 a = (Obj2 (Set a), Hom2 a a)

-- | avoid the shift key
morph :: String -> Obj2 a -> Obj2 b -> Morph2 a b
morph = Morph2

obj :: String -> a -> Obj2 a
obj = Obj2

-- | What about composition?
(&) :: (Eq a, Eq b, Eq c) => Morph2 b c -> Morph2 a b -> Morph2 a c
(&) (Morph2 n b2 c) (Morph2 m a b1)
  | b1 == b2 = Morph2 (m ++ " o " ++ n) a c
  | otherwise = error "error in composition! Cannot compose unlike objects!"
infixr 9 &

icancompose :: Morph2 Int Int
icancompose = f & g
  where f = morph "f" (obj "a" 1) (obj "b" 2)
        g = morph "g" (obj "c" 1) (obj "d" 2)

-- | dual it!
dual :: Morph2 a b -> Morph2 b a
dual (Morph2 name a b) = Morph2 ("co-" ++ name) b a

-- | the identity morphisms
idMorph :: Obj2 a -> Morph2 a a
idMorph x@(Obj2 name a) = Morph2 ("id_" ++ name) x x

-- | Now build a isomorphism
iso :: Diagram String
iso = (objs, morphs)
  where obj1 = obj "a" ""
        obj2 = obj "b" ""
        f = Morph2 "f" obj1 obj2
        g = dual f
        objs = fromList [obj1, obj2]
        morphs = fromList [f, g, idMorph obj1, idMorph obj2]
-- all this specification is bad

-- | given two object construct a diagram showing isomorphism
iso' :: (Ord a) => Obj2 a -> Obj2 a -> Diagram a
iso' a b = (fromList [a,b], morphs)
  where f = Morph2 "iso_f" a b
        g = dual f
        morphs = fromList [f, g, idMorph a, idMorph b]


-- | Lets make some different morphism
simpleMorph :: String -> Obj2 a -> Obj2 a -> Morph2 a a
simpleMorph = Morph2

functor' :: String -> Obj2 a -> Obj2 b -> Morph2 a b
functor' = Morph2

endoFunctor' :: String -> Obj2 a -> Obj2 a -> Morph2 a a
endoFunctor' = Morph2

-- | a monomorphsim is left-cancellable so f o g1 = f o g2 implies g1 == g2
isMonomorphic :: (Eq a, Eq b, Eq c) =>
  Morph2 b c -> Morph2 a b -> Morph2 a b -> Bool
isMonomorphic f g1 g2
  | f & g1 == f & g2 = True
  | otherwise = False

-- | I have no real way of constraining this
monomorph :: Morph2 a b
monomorph = undefined

epimorph :: Morph2 a b
epimorph = dual monomorph

-- OH GOD DO I NEED DEPENDENT TYPES !?!??!?!

------------------- What about higher ordered categories? -----------------------
data Obj3 a = Obj3 String a
  deriving (Show,Eq,Ord)

data Morph3 a = Morph3 String (Obj3 a) (Obj3 a)
              | NMorph (Morph3 a) (Morph3 a)
              deriving (Show,Eq,Ord)


-- Summary of problems

-- 1. With the types in try 2 it is unclear how to handle the functor case. I
-- want a functor to be Diagram a -> Diagram b -> Diagram a b, clearly I need a
-- more expressive type that can hold multiple types

-- 2. I have no way of constraining the types of different morphisms or to
-- guarentee that a monomorphism is a monomorphism. I can add boiler plate to do
-- this, or I can use dependent types to constrain these. Either way, without
-- such constraints this DSL will be boring

-- 3. Programming in this is a pain. I should figure out a monoidal type and
-- make a monoidal instance so that I can build a diagram up by more constituent
-- parts. Take iso for example, this is a super simple diagram and it already
-- does not have the form i want


-- Conclusion: Flatten the type and just create a flattened structure that can
-- describe 1-categories and 2-categories
