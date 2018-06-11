module Examples where

-- import Control.Monad.State (get)
import Debug.Trace (trace)
import Internal.Debug

import Api

five_lemma :: Sem Equ
five_lemma = do objs <- objectsAt (objectLabels 'a' 'e') (coordsX 0 0 5)
                ms <- intersperseWithLabels (labels 'f') objs
                pos <- mapM prime objs
                ms' <- intersperseWithLabels (labels 'r') pos
                lowerC 5 ms'
                res <- connectWithLabels (labels 'l') ms ms'
                setObjectLabelSize (objs ++ pos) (repeat 1.0)
                return (setFLabelSizeE 0.88 $ res)


-- product :: Sem Equ
-- product = do cntr <- objAt "$ A \times B$" (3,-3)
--              a <- objAt "$A$" (0,-3)
--              b <- objAt "$B$" (5,-3)
--              top <- objAt "$Y$" (3,0)
--              f <- uniqueArr top "$f$" cntr
