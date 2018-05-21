module Primitives where

import Diagrams.Prelude hiding (from, to, fc, (<>), adjust)
import Data.String   (IsString)

import Data.Map                       ( keys
                                      , singleton
                                      )
import Prelude hiding                 (lookup)
import Control.Arrow                  ((>>>))
import Data.Bifunctor                 (first, second)
import Data.Monoid                    ((<>))

import Types
import Core
