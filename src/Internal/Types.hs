module Internal.Types ( module Data.Map
                      , Comm(..)
                      , def
                      , Loc'
                      , Loc
                      , Obj(..)
                      , Morph(..)
                      , Morph2(..)) where


import Data.Map           (Map, empty)
import Data.Bifunctor     (Bifunctor(..))
import Data.Bifoldable    (Bifoldable(..))
import Data.Traversable   (Traversable)
import Data.Bitraversable (Bitraversable(..))
import GHC.Generics       (Generic)
import Data.Default.Class

type Table n a = Map n [a]
data Loc' a b = Loc' { _x :: a
                     , _y :: b
                     } deriving (Functor,Foldable,Traversable,Show)

instance Bifunctor Loc' where bimap f g (Loc'{_x=x,_y=y}) = Loc' (f x) (g y)
instance Bifoldable Loc' where bifoldr f g acc (Loc' {_x=x,_y=y}) = f x (g y acc)
instance Bitraversable Loc' where bitraverse f g (Loc' {_x=x,_y=y}) = Loc' <$> f x <*> g y

type Loc = Maybe (Loc' Double Double)
-- type Custom = Obj -> Obj

data Type = Homo
          | Mono
          | Epi
          | Bij
          deriving (Eq,Show,Ord)


data Obj = Obj { _name :: String              -- ^ an Objects label
               , _oPos :: Loc                 -- ^ Position of the object
               -- , _customizations :: [Custom]  -- ^ Any customizations the user wants to apply
               , _frozen :: Bool              -- ^ Is the object able to be changed?
               } deriving Show

data Morph = Morph { _mLabel :: String              -- ^ the label for the arrow
                   , _mTo    :: Obj                 -- ^ The object the arrow points to
                   , _mPos   :: Loc                 -- ^ Position of the arrow
                   , _types  :: [Type]              -- ^ The type of the arrow
                   -- , _mCustomizations :: [Custom]   -- ^ Any customizations the user wants to apply
                   } deriving (Generic,Show)

data Morph2 = Morph2 { _m2Label :: String             -- ^ the label for the arrow
                     , _m2To    :: Morph              -- ^ The object the arrow points to
                     , _m2Pos   :: Loc                -- ^ Position of the natural transformation
                     , _m2Types :: [Type]             -- ^ The type of the arrow
                     -- , _m2Customizations :: [Custom]  -- ^ Any customizations the user wants to apply
                     } deriving (Generic,Show)

type Cat1 = Table Obj Morph
type Cat2 = Table Morph Morph2

-- | The Semantic Value for the DSL
newtype Comm = C {unC :: (Cat1, Cat2)} deriving (Generic,Show)

instance Eq Obj where Obj{_name=n} == Obj{_name=m} = n == m
instance Ord Obj where compare Obj{_name=n} Obj{_name=m} = compare n m
instance Default Obj where
  def = Obj {_name=def,_oPos=Nothing
            -- ,_customizations=[]
            ,_frozen=False}
instance Default Morph
instance Default Morph2
instance Default (Map k v) where def = empty
instance Default Comm
