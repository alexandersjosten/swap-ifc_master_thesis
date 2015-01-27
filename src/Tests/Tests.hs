module Tests.Tests where

import Test.QuickCheck
import System.IO.Unsafe
import SwapIFC

------------------------------------------------------------------------------
------------- Data types and generators for types and operations -------------
------------------------------------------------------------------------------

data Types
  = Num
  | Frac
  | Bool
  | Eq
  | Ord
  deriving Show

instance Arbitrary Types where
  arbitrary = genTypes

genTypes :: Gen Types
genTypes = oneof [ return Num
                 , return Frac
                 , return Bool
                 , return Eq
                 , return Ord
                 ]

data NumOp
  = Add
  | Sub
  | Mul
  | Neg
  | Abs
  | Sig

instance Arbitrary NumOp where
  arbitrary = genNumOp

genNumOp :: Gen NumOp
genNumOp = oneof [ return Add
                 , return Sub
                 , return Mul
                 , return Neg
                 , return Abs
                 , return Sig
                 ]

data FracOp
  = Div
  | Recip
  | FromRational

instance Arbitrary FracOp where
  arbitrary = genFracOp

genFracOp :: Gen FracOp
genFracOp = oneof [ return Div
                  , return Recip
                  , return FromRational
                  ]

data BoolOp
  = And
  | Or
  | Not

instance Arbitrary BoolOp where
  arbitrary = genBoolOp

genBoolOp :: Gen BoolOp
genBoolOp = oneof [ return And
                  , return Or
                  , return Not
                  ]

data EqOp
  = Equals
  | NotEquals

instance Arbitrary EqOp where
  arbitrary = genEqOp

genEqOp :: Gen EqOp
genEqOp = oneof [ return Equals
                , return NotEquals
                ]

data OrdOp
  = Less
  | GreaterEquals
  | LessEquals
  | Greater
  | Max
  | Min

instance Arbitrary OrdOp where
  arbitrary = genOrdOp

genOrdOp :: Gen OrdOp
genOrdOp = oneof [ return Less
                 , return GreaterEquals
                 , return LessEquals
                 , return Greater
                 , return Max
                 , return Min
                 ]

genFlowNumExpr :: (Num a) => Gen (Flow t a)
genFlowNumExpr = undefined

genFlowFracExpr :: (Fractional a) => Gen (Flow t a)
genFlowFracExpr = undefined

genFlowBoolExpr :: Gen (Flow t Bool)
genFlowBoolExpr = undefined

genFlowEqExpr :: (Eq a) => Gen (Flow t a)
genFlowEqExpr = undefined

genFlowOrdExpr :: (Ord a) => Gen (Flow t a)
genFlowOrdExpr = undefined

------------------------------------------------------------------------------
-------------------------------- Test Program --------------------------------
------------------------------------------------------------------------------

prop_testSwapIFC :: Property
prop_testSwapIFC = undefined

{-
prop_testSwapIFC :: Types -> Property
prop_testSwapIFC t = collect t (True)

testGetType :: Gen Types -> Types
testGetType = unsafePerformIO . generate


genBool :: Gen Bool
genBool = oneof [return True, return False]

genInt :: Gen Int
genInt = oneof $ map return [minBound :: Int .. maxBound :: Int]

prop_testBool :: Bool -> Property
prop_testBool b = collect b True
-}
