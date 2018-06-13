module Examples where

-- import Control.Monad.State (get)
import Debug.Trace (trace)
import Control.Arrow ((***))
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

iDontCompose :: Sem Equ
iDontCompose =
  do f <- arrowAt "A" "f" "B" (0,0) (2,0)
     g <- arrowAt "B" "g" "C" (2,0) (2,-2)
     pure <$> f `compM` g

iCompose :: Sem Equ
iCompose = do f <- arrowAt "A" "f" "B" (0,0) (2,0)
              g <- arrowAt "B" "g" "C" (2,0) (2,-2)
              pure <$> g `compM` f

-- | Will throw an uncaught error at compile time because A and B are in the
-- same location
iThrowAnUnCaughtError :: Sem Equ
iThrowAnUnCaughtError = do a <- arrowAt "A" "f" "B" (0,0) (0,0)
                           return . pure . pure $ a

product2L :: Sem Equ
product2L = do
  let [axb, a, y]    = [mathify "A \\times B", mathify "A", mathify "Y"]
      [axbC, aC, yC] = [(0,0)                , (-3,0)     , (0,3)      ]
  f <- arrowAt y   "f" axb yC   axbC
  g <- arrowAt axb "g" a   axbC aC
  h <- arrowAt y   "h" a   yC   aC
  p <- (g `compM` f) |==| (liftToComp h)
  return p

product2R :: Sem Equ
product2R = do p' <- product2L >>= return . swapMLabelE "g" (const "j") >>= return . swapMLabelE "h" (const "i")
               newP <- swapLabelE (mathify "A") (const $ mathify "B") p'
               flipOverE (negate *** id) newP
               return newP

-- throws an error because of a bug with the latex. bug turns "$A \\times B$"
-- into "$A \times B$" and therefore produces an except from Data.Map in the
-- sem' function in Sem module.
product2 :: Sem Equ
product2 = (coEqu <$> product2L) `mergeE` (coEqu <$> product2R)

sumType1 :: Sem Equ
sumType1 = coEqu <$> product_

sumType :: Sem Equ
sumType = coEqu <$> product_ >>= swapLabelE ("$A \\times B$") (const $ mathify "A \\coprod B")
