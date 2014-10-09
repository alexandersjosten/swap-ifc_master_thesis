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
  return = Flow . return

  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: Flow tag a -> (a -> Flow tag b) -> Flow tag b
  -- (>>=) :: Flow tag (IO a) -> (a -> Flow tag (IO b)) -> Flow tag (IO b)
  (Flow ioa) >>= f = Flow $ do
    a <- ioa
    case (f a) of
      Flow iob -> iob

-- Functor
instance Functor (Flow tag) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Flow ioa) = Flow $ ioa >>= return . f

-- Applicative
instance Applicative (Flow tag) where
  -- pure :: a -> f a
  pure = return

  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = ap
