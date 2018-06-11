module Examples where

import Data.Char  (toUpper)
import Control.Monad.State (get)
import Debug.Trace (trace)

import Api

five_lemma :: Sem Equ
five_lemma = do objs <- objectsAt (objectLabels 'a' 'e') (coordsX 0 0 5)
                ms <- intersperseWithLabels (labels 'f') objs
                pos <- mapM prime objs
                ms' <- intersperseWithLabels (labels 'r') pos
                lower 5 ms'
                st <- get
                trace (show st) $ return ()
                fmap pure $ connectWithLabels (labels 'l') ms ms'
