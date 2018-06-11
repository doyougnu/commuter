module Examples where

-- import Control.Monad.State (get)
import Debug.Trace (trace)
import Internal.Debug

import Api

five_lemma :: Sem Equ
five_lemma = do objs <- objectsAt (objectLabels 'a' 'e') (coordsX (-5) 0 8)
                ms <- intersperseWithLabels (labels 'f') objs
                pos <- mapM prime objs
                ms' <- intersperseWithLabels (labels 'r') pos
                lowerC 8 ms'
                res <- connectWithLabels (labels 'l') ms ms'
                setObjectsLabelSize (objs ++ pos) (repeat 1.0)
                return (setFLabelSizeE 0.88 $ res)


productL :: Sem Equ
productL = do let axb = "$A \\times B$"
                  aL  = "$A$"
                  topL = "$Y$"
              _ <- objectsAt [axb,aL,topL] [(0,0), (-3,0), (0,3)]
              arrs <- arrows [topL,axb,topL] (labels 'f') [axb,aL,aL]
              return . pure $ arrs

productR :: Sem Equ
productR = do let axb = "$A \\times B$"
                  bL  = "$B$"
                  topL = "$Y$"
              _ <- objectsAt [axb,bL,topL] [(0,0), (3,0), (0,3)]
              arrs <- arrows [topL,axb,topL] (labels 'i') [axb,bL,bL]
              return . pure $ arrs

product_ :: Sem Equ
product_ = productL `mergeE` productR


sumType1 :: Sem Equ
sumType1 = coEqu <$> product_

sumType :: Sem Equ
sumType = do p <- coEqu <$> product_
             st <- get
             trace (show st) $ return ()
             p' <- swapLabelE ("$A \\times B$") (const $ mathify "A \\coprod B") p
             st' <- get
             trace (show st') $ return ()
             return p'
