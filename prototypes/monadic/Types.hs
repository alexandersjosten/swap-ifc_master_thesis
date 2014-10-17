{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Types where

import Control.Applicative
import Control.Monad

-- | High datatype for private values.
data High

-- | Low datatype for public values.
data Low

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

--------------------------------------------------------------------------------

-- Num instance for Flow
infixl 7 .*.
infixl 6 .+., .-.
class FlowNum t1 t2 t3 | t1 t2 -> t3 where
  -- | Binary addition operator for Flow
  (.+.) :: Num a => Flow t1 a -> Flow t2 a -> Flow t3 a
  (.+.) = calcNumFlow (+)

  -- | Binary multiplication operator for Flow
  (.*.) :: Num a => Flow t1 a -> Flow t2 a -> Flow t3 a
  (.*.) = calcNumFlow (*)

  -- | Binary subtraction operator for Flow
  (.-.) :: Num a => Flow t1 a -> Flow t2 a -> Flow t3 a
  (.-.) = calcNumFlow (-)

  -- | Unary negation for Flow
  fNeg :: Num a => Flow t1 a -> Flow t1 a
  fNeg = appNumFlow negate

  -- | Unary absolute value for Flow
  fAbs :: Num a => Flow t1 a -> Flow t1 a
  fAbs = appNumFlow abs

  -- | Unary signum for Flow
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

--------------------------------------------------------------------------------

-- Bool instance for Flow
infixr 3 .&&.
infixr 2 .||.
class FlowBool t1 t2 t3 | t1 t2 -> t3 where
  -- | Binary logic AND for Flow
  (.&&.) :: Flow t1 Bool -> Flow t2 Bool -> Flow t3 Bool
  (.&&.) = compareFlow (&&)

  -- | Binary logic OR for Flow
  (.||.) :: Flow t1 Bool -> Flow t2 Bool -> Flow t3 Bool
  (.||.) = compareFlow (||)

  -- | Unary logic NOT for Flow
  fNot :: Flow t1 Bool -> Flow t1 Bool
  fNot (Flow ioa) = Flow $ do
    a <- ioa
    return $ not a

instance FlowBool High High High
instance FlowBool High Low High
instance FlowBool Low High High
instance FlowBool Low Low Low

compareFlow :: (Bool -> Bool -> Bool)
            -> Flow t1 Bool
            -> Flow t2 Bool
            -> Flow t3 Bool
compareFlow op (Flow io1) (Flow io2) = Flow $ do
  a1 <- io1
  a2 <- io2
  return $ op a1 a2

--------------------------------------------------------------------------------
