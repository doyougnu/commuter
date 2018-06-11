module Examples where

import Control.Monad.State (get)
import Debug.Trace (trace)

import Api

five_lemma :: Sem Equ
five_lemma = do objs <- objectsAt (objectLabels 'a' 'e') (coordsX 0 0 5)
                ms <- intersperseWithLabels (labels 'f') objs
                pos <- mapM prime objs
                ms' <- intersperseWithLabels (labels 'r') pos
                lowerC 5 ms'
                res <- connectWithLabels (labels 'l') ms ms'
                return res
