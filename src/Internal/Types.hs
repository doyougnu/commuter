module Internal.Types ( def
                      , Loc'(..)
                      , Loc
                      , Obj(..)
                      , Morph'(..)
                      , Morph2(..)
                      , Equation(..)
                      , Comm) where


import           Data.Bifoldable    (Bifoldable (..))
import           Data.Bifunctor     (Bifunctor (..))
import           Data.Bitraversable (Bitraversable (..))
import           Data.Default.Class
import           Data.Traversable   (Traversable)
import           GHC.Generics       (Generic)

data Loc' a b = Loc' { _x :: a
                     , _y :: b
                     } deriving (Eq,Functor,Foldable,Traversable,Show)

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


data Obj = Obj { _name   :: String              -- ^ an Objects label
               , _oPos   :: Loc                 -- ^ Position of the object
               -- , _customizations :: [Custom]  -- ^ Any customizations the user wants to apply
               , _frozen :: Bool              -- ^ Is the object able to be changed?
               , _fSize  :: Double            -- ^ Font size of the object label
               } deriving Show

data Morph' = Morph' { _mFrom  :: Obj                 -- ^ The object that originates the arrow
                     , _mLabel :: String              -- ^ the label for the arrow
                     , _mTo    :: Obj                 -- ^ The object the arrow points to
                     , _mPos   :: Loc                 -- ^ Position of the arrow
                     , _types  :: [Type]              -- ^ The type of the arrow
                     -- , _mCustomizations :: [Custom]   -- ^ Any customizations the user wants to apply
                     } deriving (Generic,Show)

data Morph2 = Morph2 { _m2From  :: Morph'            -- ^ The arrow the arrow points from
                     , _m2Label :: String             -- ^ the label for the arrow
                     , _m2To    :: Morph'              -- ^ The arrow the arrow points to
                     , _m2Pos   :: Loc                -- ^ Position of the natural transformation
                     , _m2Types :: [Type]             -- ^ The type of the arrow
                     -- , _m2Customizations :: [Custom]  -- ^ Any customizations the user wants to apply
                     } deriving (Generic,Show)


-- | The Semantic Value for the DSL, a Morph is really just an equation
data Equation = E Morph'
              | Equation :.: Equation
              | Equation :=: Equation
              deriving Show

type Comm = [Equation]

instance Eq Obj where Obj{_name=n} == Obj{_name=m} = n == m
instance Ord Obj where compare Obj{_name=n} Obj{_name=m} = compare n m
instance Default Obj where
  def = Obj {_name=def
            ,_oPos=Nothing
            -- ,_customizations=[]
            ,_frozen=False
            , _fSize = 0.22}
instance Default Morph'
instance Default Morph2
instance (Default a, Default b) => Default (Loc' a b)
  where def = Loc' def def
