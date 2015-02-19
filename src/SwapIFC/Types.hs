{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GADTs, CPP,
    FlexibleInstances, FlexibleContexts, TypeFamilies #-}

module SwapIFC.Types where

import SwapIFC.Haste.CodeGen
import Control.Applicative
import Control.Monad
import Data.IORef

-- | High datatype for private values.
data High

-- | Low datatype for public values.
data Low

-- | The Flow type keeps track of the information flow.
newtype Flow tag a = Flow (IO a)

newtype FlowRef tag a = FlowRef (IORef a)

-- Monadic stuff for High flow..
instance Monad (Flow High) where
  -- return :: a -> m a
#ifdef __HASTE__
  return = Flow . upg
#else
  return = Flow . return
#endif

  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: Flow tag a -> (a -> Flow tag b) -> Flow tag b
  -- (>>=) :: Flow tag (IO a) -> (a -> Flow tag (IO b)) -> Flow tag (IO b)
  (Flow ioa) >>= f = Flow $ do
    a <- ioa
    case (f a) of
      Flow iob -> iob

-- Functor
instance Functor (Flow High) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f ioa = ioa >>= return . f

-- Applicative
instance Applicative (Flow High) where
  -- pure :: a -> f a
  pure = return

  -- (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = ap

-- Monadic stuff for Low flow..
instance Monad (Flow Low) where
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
instance Functor (Flow Low) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Flow ioa) = Flow $ ioa >>= return . f

-- Applicative
instance Applicative (Flow Low) where
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
  fNeg :: (Num a, t1~t2, t1~t3) => Flow t1 a -> Flow t3 a
  fNeg = appNumFlow negate

  -- | Unary absolute value for Flow
  fAbs :: (Num a, t1~t2, t1~t3) => Flow t1 a -> Flow t3 a
  fAbs = appNumFlow abs

  -- | Unary signum for Flow
  fSig :: (Num a, t1~t2, t1~t3) => Flow t1 a -> Flow t3 a
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

infixl 7 ./.
class FlowFrac t1 t2 t3 | t1 t2 -> t3 where
  -- | Binary division operator for Flow
  (./.) :: Fractional a => Flow t1 a -> Flow t2 a -> Flow t3 a
  (./.) = calcFracFlow (/)

  fRecip :: (Fractional a, t1~t2, t1~t3) => Flow t1 a -> Flow t3 a
  fRecip (Flow ioa) = Flow $ do
    a <- ioa
    return $ recip a

  fFromRational :: (Fractional a, t1~t2, t1~t3) => Flow t1 Rational -> Flow t3 a
  fFromRational (Flow ior) = Flow $ do
    r <- ior
    return $ fromRational r

instance FlowFrac High High High
instance FlowFrac High Low High
instance FlowFrac Low High High
instance FlowFrac Low Low Low

calcFracFlow :: Fractional a => (a -> a -> a) -> Flow t1 a -> Flow t2 a -> Flow t3 a
calcFracFlow op (Flow ioa1) (Flow ioa2) = Flow $ do
  a1 <- ioa1
  a2 <- ioa2
  return $ op a1 a2

--------------------------------------------------------------------------------

-- Bool instance for Flow
infixr 3 .&&.
infixr 2 .||.
class FlowBool t1 t2 t3 | t1 t2 -> t3 where
  -- | Binary logic AND for Flow
  (.&&.) :: Flow t1 Bool -> Flow t2 Bool -> Flow t3 Bool
  (.&&.) = logicBoolFlow (&&)

  -- | Binary logic OR for Flow
  (.||.) :: Flow t1 Bool -> Flow t2 Bool -> Flow t3 Bool
  (.||.) = logicBoolFlow (||)

  -- | Unary logic NOT for Flow
  fNot ::(t1~t2) => Flow t1 Bool -> Flow t3 Bool
  fNot (Flow ioa) = Flow $ do
    a <- ioa
    return $ not a

instance FlowBool High High High
instance FlowBool High Low High
instance FlowBool Low High High
instance FlowBool Low Low Low

logicBoolFlow :: (Bool -> Bool -> Bool)
              -> Flow t1 Bool
              -> Flow t2 Bool
              -> Flow t3 Bool
logicBoolFlow op (Flow io1) (Flow io2) = Flow $ do
  a1 <- io1
  a2 <- io2
  return $ op a1 a2

--------------------------------------------------------------------------------

-- Eq instance for Flow
infixl 4 .==., ./=., .<., .<=., .>., .>=.
class FlowEqOrd t1 t2 t3 | t1 t2 -> t3 where
  -- | Binary equals for Flow
  (.==.) :: Eq a => Flow t1 a -> Flow t2 a -> Flow t3 Bool
  (.==.) = compareFlow (==)

  -- | Binary not equals for Flow
  (./=.) :: Eq a => Flow t1 a -> Flow t2 a -> Flow t3 Bool
  (./=.) = compareFlow (/=)

  -- | Binary less than for Flow
  (.<.) :: Ord a => Flow t1 a -> Flow t2 a -> Flow t3 Bool
  (.<.) = compareFlow (<)

  -- | Binary greater equals
  (.>=.) :: Ord a => Flow t1 a -> Flow t2 a -> Flow t3 Bool
  (.>=.) = compareFlow (>=)

  -- | Binary less equals
  (.<=.) :: Ord a => Flow t1 a -> Flow t2 a -> Flow t3 Bool
  (.<=.) = compareFlow (<=)

  -- | Binary greater than
  (.>.) :: Ord a => Flow t1 a -> Flow t2 a -> Flow t3 Bool
  (.>.) = compareFlow (>)

  -- | max function for Flow
  fMax :: Ord a => Flow t1 a -> Flow t2 a -> Flow t3 a
  fMax = maxMinFlow max

  -- | min function for Flow
  fMin :: Ord a => Flow t1 a -> Flow t2 a -> Flow t3 a
  fMin = maxMinFlow min

instance FlowEqOrd High High High
instance FlowEqOrd High Low High
instance FlowEqOrd Low High High
instance FlowEqOrd Low Low Low

compareFlow :: (a -> a -> Bool) -> Flow t1 a -> Flow t2 a -> Flow t3 Bool
compareFlow op (Flow ioa) (Flow iob) = Flow $ do
  a <- ioa
  b <- iob
  return $ op a b

maxMinFlow :: (a -> a -> a) -> Flow t1 a -> Flow t2 a -> Flow t3 a
maxMinFlow op (Flow ioa) (Flow iob) = Flow $ do
  a <- ioa
  b <- iob
  return $ op a b

--------------------------------------------------------------------------------
-- For printing, think lprint in JSFlow (not really dependent on context though)
class (Monad (Flow t)) => Tag t where
  tagRep :: Flow t String

instance Tag High where
  tagRep = return "_<H>"

instance Tag Low where
  tagRep = return "_<L>"

--------------------------------------------------------------------------------
-- Used for type contstraints in e.g. run
class ValidFlow t1 t2 where
  type Result t1 t2

instance ValidFlow Low High where
  type Result Low High = High
instance ValidFlow High High where
  type Result High High = High
instance ValidFlow Low Low where
  type Result Low Low = Low

--------------------------------------------------------------------------------
-- Used for type contstraints in e.g. run
class CombineFlow t1 t2 where
  type CombineResult t1 t2

instance CombineFlow Low High where
  type CombineResult Low High = High
instance CombineFlow High High where
  type CombineResult High High = High
instance CombineFlow Low Low where
  type CombineResult Low Low = Low
instance CombineFlow High Low where
  type CombineResult High Low = High

--------------------------------------------------------------------------------
