{-# LANGUAGE GADTs #-}

module Types where

import Control.Applicative
import Control.Monad

-- | High datatype for private values.
data High = High

-- | Low datatype for public values.
data Low = Low

-- | The Flow type keeps track of the information flow.
newtype Flow tag a = Flow (IO a)

-- Monadic stuff..
instance Monad (Flow tag) where
  -- return :: a -> m a
  return a = undefined

  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) a f = undefined

-- Functor
instance Functor (Flow tag) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = undefined

-- Applicative
instance Applicative (Flow tag) where
  -- pure :: a -> f a
  pure = return

  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = ap
