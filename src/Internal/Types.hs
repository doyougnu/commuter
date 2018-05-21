module Types ( module Data.Map
             , module Data.String
             , LocTable
             , Graph(..)
             , LGraph(..)
             , Edge
             , Adj
             , NAdj
             , EAdj) where

import Data.Map (Map, empty)
import Data.String (IsString)

type Table n a = Map n a
type Loc = (,) Double Double
type LocTable n = Map n Loc

type Adj n l = Table n [(l, n)]        -- ^ an adjacency table via ns
type NAdj n l = Adj n l                -- ^ node lbls n, func lbls l
type EAdj l m = Adj l m                -- ^ func labels l, hfunc lbls m

newtype Graph n m l = G { unG :: (Adj n l, EAdj l m)} -- ^ this graph can have arrows
  deriving (Show,Eq)                                  -- ^ between nodes and arrows
                                                      -- ^ between edges

type Edge n l = (n, l, n)                   -- ^ abstracted edges, w/ labels

-- | Label is a function from Nodes to Labels
type Label n l = n -> l
data LGraph nl n m l = LGraph (Graph n m l) (Label nl l)

instance (Ord n, Ord l) => Monoid (Graph n m l) where
  mempty = G (empty, empty)
  mappend (G (a, b)) (G (c, d)) = G (a `mappend` c, b `mappend` d)
