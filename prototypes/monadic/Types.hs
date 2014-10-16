{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

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

infixl 7 .*.
infixl 6 .+., .-.
-- Num instances for Flow
class FlowNum t1 t2 t3 | t1 t2 -> t3 where
  (.+.) :: Num a => Flow t1 a -> Flow t2 a -> Flow t3 a
  (.+.) = calcNumFlow (+)

  (.*.) :: Num a => Flow t1 a -> Flow t2 a -> Flow t3 a
  (.*.) = calcNumFlow (*)
  
  (.-.) :: Num a => Flow t1 a -> Flow t2 a -> Flow t3 a
  (.-.) = calcNumFlow (-)

  fNeg :: Num a => Flow t1 a -> Flow t1 a
  fNeg = appNumFlow negate

  fAbs :: Num a => Flow t1 a -> Flow t1 a
  fAbs = appNumFlow abs

  fSig :: Num a => Flow t1 a -> Flow t1 a
  fSig = appNumFlow signum

instance FlowNum High High High
instance FlowNum High Low High
instance FlowNum Low High High
instance FlowNum Low Low Low

calcNumFlow :: Num a => (a -> a -> a) -> Flow t1 a -> Flow t2 a -> Flow t3 a
calcNumFlow op (Flow ioa1) (Flow ioa2) = Flow $ do
  a1 <- ioa1
  a2 <- ioa2
  return $ op a1 a2

appNumFlow :: Num a => (a -> a) -> Flow t1 a -> Flow t1 a
appNumFlow f (Flow ioa) = Flow $ do
  a <- ioa
  return $ f a
