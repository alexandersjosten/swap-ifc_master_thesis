{-# LANGUAGE GADTs #-}

module Types where

import Data.Map
import Control.Applicative
import Control.Monad
import Control.Monad.State

-- | If the level is High, it means private. Low means public.
data Level = High | Low

type Ident = String
type Scope a = Map Ident (Level, Term a)

-- Internal state, needs to keep track of program counter and the active scopes
-- (perhaps more stuff will be added..)
data InternalState a = InState { pc :: Level
                               , activeScopes :: [Scope a]
                               }

-- | The Flow type keeps track of the information flow.
newtype Flow level a = Flow (State (level, a) (InternalState a))

-- | All the terms are defined within the Term datatype.
data Term a where
  Var    :: Term a -> Term a
  FunApp :: Term (a -> b) -> Term a -> Term b


-- Monadic stuff..
instance Monad (Flow a) where
  -- return :: a -> m a
  return a = undefined

  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) a f = undefined

-- Functor
instance Functor (Flow a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = undefined

-- Applicative
instance Applicative (Flow a) where
  -- pure :: a -> f a
  pure = return

  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = ap
