{-# LANGUAGE GADTs #-}

module Types where

import Control.Applicative
import Control.Monad

-- | If the level is High, it means private. Low means public.
data Level = High | Low


-- | The Flow type keeps track of the information flow.
newtype Flow level a = Flow (State (level, a) (InternalState a))


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
