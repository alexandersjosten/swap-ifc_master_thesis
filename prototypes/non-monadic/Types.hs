{-# LANGUAGE GADTs #-}

module Types where

import Data.Map
import Control.Monad.State

data Tag = Low | High

type Ident = String
type Scope a = Map Ident (Tag, Term a)

data InternalState a = InState { pc :: Tag
                               , activeScopes :: [Scope a]
                               }

newtype Flow level a = Flow (State (level, a) (InternalState a))

data LType a where
  LInt    :: Tag -> Int    -> LType (Tag, Int)
  LString :: Tag -> String -> LType (Tag, String)
  LBool   :: Tag -> Bool   -> LType (Tag, Bool)

data Term a where
  TType :: Term (LType a)
  TApp  :: Term a -> Term a
