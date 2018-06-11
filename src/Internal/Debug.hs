module Internal.Debug where

import Internal.Core (emptySt)
import Control.Monad.State (evalState)
import Control.Monad.Except (runExceptT)

import Internal.Types

debugO' :: Obj -> String
debugO' Obj{..} = spacer ++ _name ++ ": \n  " ++ spacer ++ show _frozen
                  ++ spacer ++ show _fSize ++ spacer
  where spacer = "  \n  "

debugM' :: Morph -> String
debugM' Morph{..} = spacer ++ _mLabel ++ ": \n  " ++ _mFrom  ++ spacer
                    ++ _mTo  ++ spacer ++ show _types ++ spacer ++ show _mfSize
                    ++ spacer
  where spacer = " \n  "

debugC' :: Comp -> String
debugC' = Prelude.foldr (++) "" . fmap debugM'

debugE' :: Equ -> String
debugE' = Prelude.foldr (++) "" . fmap debugC'

debugO :: Sem Obj -> String
debugO = debugMaker go
  where
    go (Right a) = debugO' a
    go (Left err) = show err

debugMaker :: (Either ErrMsg a -> String) -> Sem a -> String -- fmap?
debugMaker f = f . flip evalState emptySt . runExceptT

debugM :: Sem Morph -> String
debugM = debugMaker go
  where
    go (Right a) = debugM' a
    go (Left err) = show err

debugC :: Sem Comp -> String
debugC = debugMaker go
  where
    go (Right a) = debugC' a
    go (Left err) = show err

debugE :: Sem Equ -> String
debugE = debugMaker go
  where
    go (Right a) = debugE' a
    go (Left err) = show err
