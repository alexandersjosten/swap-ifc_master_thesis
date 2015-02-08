module Tests.Tests where

import Test.QuickCheck hiding ((.||.), (.&&.))
import System.IO.Unsafe
import SwapIFC
import qualified SwapIFC.Types as T

-- Instance needed for quickCheck to be happy..
instance (T.Tag t, Show a) => Show (Flow t a) where
  show flow = unsafeShow flow

------------------------------------------------------------------------------
------------------ Data types and generators for operations ------------------
------------------------------------------------------------------------------

data NumOp
  = Add
  | Sub
  | Mul
  | Neg
  | Abs
  | Sig
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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


------------------------------------------------------------------------------
--------------------------------- Properties ---------------------------------
------------------------------------------------------------------------------

prop_lowNumExpr :: (NumOp, Int, Int) -> Bool
prop_lowNumExpr (op, val1, val2) =
  case op of
    Add -> (show (flow1 .+. flow2) == show (flow2 .+. flow1)) &&
           show (flow1 .+. flow2) == show (val1 + val2) ++ "_<L>"
    Sub -> show (flow1 .-. flow2) == show (val1 - val2) ++ "_<L>" &&
           show (flow2 .-. flow1) == show (val2 - val1) ++ "_<L>"
    Mul -> (show (flow1 .*. flow2) == show (flow2 .*. flow1)) &&
           show (flow1 .*. flow2) == show (val1 * val2) ++ "_<L>"
    Neg -> show (fNeg flow1) == show (negate val1) ++ "_<L>"
    Abs -> show (fAbs flow1) == show (abs val1) ++ "_<L>"
    Sig -> show (fSig flow1) == show (signum val1) ++ "_<L>"
  where flow1 = mkLow val1
        flow2 = mkLow val2

prop_highNumExpr :: (NumOp, Int, Int) -> Bool
prop_highNumExpr (op, val1, val2) =
  case op of
    Add -> (show (flow1 .+. flow2) == show (flow2 .+. flow1)) &&
           show (flow1 .+. flow2) == show (val1 + val2) ++ "_<H>"
    Sub -> show (flow1 .-. flow2) == show (val1 - val2) ++ "_<H>" &&
           show (flow2 .-. flow1) == show (val2 - val1) ++ "_<H>"
    Mul -> (show (flow1 .*. flow2) == show (flow2 .*. flow1)) &&
           show (flow1 .*. flow2) == show (val1 * val2) ++ "_<H>"
    Neg -> show (fNeg flow1) == show (negate val1) ++ "_<H>"
    Abs -> show (fAbs flow1) == show (abs val1) ++ "_<H>"
    Sig -> show (fSig flow1) == show (signum val1) ++ "_<H>"
  where flow1 = mkHigh val1
        flow2 = mkHigh val2

prop_mixedNumExpr :: (NumOp, Int, Int) -> Bool
prop_mixedNumExpr (op, val1, val2) =
  case op of
    Add -> (show (flow1 .+. flow2) == show (flow2 .+. flow1)) &&
           show (flow1 .+. flow2) == show (val1 + val2) ++ "_<H>"
    Sub -> show (flow1 .-. flow2) == show (val1 - val2) ++ "_<H>" &&
           show (flow2 .-. flow1) == show (val2 - val1) ++ "_<H>"
    Mul -> (show (flow1 .*. flow2) == show (flow2 .*. flow1)) &&
           show (flow1 .*. flow2) == show (val1 * val2) ++ "_<H>"
    Neg -> show (fNeg flow1) == show (negate val1) ++ "_<H>"
    Abs -> show (fAbs flow1) == show (abs val1) ++ "_<H>"
    Sig -> show (fSig flow1) == show (signum val1) ++ "_<H>"
  where flow1 = mkHigh val1
        flow2 = mkLow val2


prop_lowFracExpr :: (FracOp, Double, Double) -> Bool
prop_lowFracExpr (op, val1, val2) =
  case op of
    Div          -> show (flow1 ./. flow2) == show (val1 / val2) ++ "_<L>" &&
                    show (flow2 ./. flow1) == show (val2 / val1) ++ "_<L>"
    Recip        -> show (fRecip flow1) == show (recip val1) ++ "_<L>"
    FromRational -> True --show (fFromRational flow1) == show (fromRational val1) ++ "_<L>"
  where flow1 = mkLow val1
        flow2 = mkLow val2

prop_highFracExpr :: (FracOp, Double, Double) -> Bool
prop_highFracExpr (op, val1, val2) =
  case op of
    Div          -> show (flow1 ./. flow2) == show (val1 / val2) ++ "_<H>" &&
                    show (flow2 ./. flow1) == show (val2 / val1) ++ "_<H>"
    Recip        -> show (fRecip flow1) == show (recip val1) ++ "_<H>"
    FromRational -> True --show (fFromRational flow1) == show (fromRational val1) ++ "_<H>"
  where flow1 = mkHigh val1
        flow2 = mkHigh val2

prop_mixedFracExpr :: (FracOp, Double, Double) -> Bool
prop_mixedFracExpr (op, val1, val2) =
  case op of
    Div          -> show (flow1 ./. flow2) == show (val1 / val2) ++ "_<H>" &&
                    show (flow2 ./. flow1) == show (val2 / val1) ++ "_<H>"
    Recip        -> show (fRecip flow1) == show (recip val1) ++ "_<H>"
    FromRational -> True --show (fFromRational flow1) == show (fromRational val1) ++ "_<H>"
  where flow1 = mkHigh val1
        flow2 = mkLow val2



prop_lowBoolExpr :: (BoolOp, Bool, Bool) -> Bool
prop_lowBoolExpr (op, val1, val2) =
  case op of
    And -> show (flow1 .&&. flow2) == show (val1 && val2) ++ "_<L>" &&
           show (flow2 .&&. flow1) == show (val2 && val1) ++ "_<L>" &&
           show (flow1 .&&. flow2) == show (flow2 .&&. flow1)
    Or  -> show (flow1 .||. flow2) == show (val1 || val2) ++ "_<L>" &&
           show (flow2 .||. flow1) == show (val2 || val1) ++ "_<L>" &&
           show (flow1 .||. flow2) == show (flow2 .||. flow1)
    Not -> show (fNot flow1) == show (not val1) ++ "_<L>" &&
           show (fNot flow2) == show (not val2) ++ "_<L>"
  where flow1 = mkLow val1
        flow2 = mkLow val2

prop_highBoolExpr :: (BoolOp, Bool, Bool) -> Bool
prop_highBoolExpr (op, val1, val2) =
  case op of
    And -> show (flow1 .&&. flow2) == show (val1 && val2) ++ "_<H>" &&
           show (flow2 .&&. flow1) == show (val2 && val1) ++ "_<H>" &&
           show (flow1 .&&. flow2) == show (flow2 .&&. flow1)
    Or  -> show (flow1 .||. flow2) == show (val1 || val2) ++ "_<H>" &&
           show (flow2 .||. flow1) == show (val2 || val1) ++ "_<H>" &&
           show (flow1 .||. flow2) == show (flow2 .||. flow1)
    Not -> show (fNot flow1) == show (not val1) ++ "_<H>" &&
           show (fNot flow2) == show (not val2) ++ "_<H>"
  where flow1 = mkHigh val1
        flow2 = mkHigh val2


prop_mixedBoolExpr :: (BoolOp, Bool, Bool) -> Bool
prop_mixedBoolExpr (op, val1, val2) =
  case op of
    And -> show (flow1 .&&. flow2) == show (val1 && val2) ++ "_<H>" &&
           show (flow2 .&&. flow1) == show (val2 && val1) ++ "_<H>" &&
           show (flow1 .&&. flow2) == show (flow2 .&&. flow1)
    Or  -> show (flow1 .||. flow2) == show (val1 || val2) ++ "_<H>" &&
           show (flow2 .||. flow1) == show (val2 || val1) ++ "_<H>" &&
           show (flow1 .||. flow2) == show (flow2 .||. flow1)
    Not -> show (fNot flow1) == show (not val1) ++ "_<H>" &&
           show (fNot flow2) == show (not val2) ++ "_<L>"
  where flow1 = mkHigh val1
        flow2 = mkLow val2


prop_lowEqExpr :: (EqOp, Bool, Bool) -> Bool
prop_lowEqExpr (op, val1, val2) =
  case op of
    Equals     -> show (flow1 .==. flow2) == show (val1 == val2) ++ "_<L>" &&
                  show (flow2 .==. flow1) == show (val2 == val1) ++ "_<L>" &&
                  show (flow1 .==. flow2) == show (flow2 .==. flow1)
    NotEquals  -> show (flow1 ./=. flow2) == show (val1 /= val2) ++ "_<L>" &&
                  show (flow2 ./=. flow1) == show (val2 /= val1) ++ "_<L>" &&
                  show (flow1 ./=. flow2) == show (flow2 ./=. flow1)
  where flow1 = mkLow val1
        flow2 = mkLow val2

prop_highEqExpr :: (EqOp, Bool, Bool) -> Bool
prop_highEqExpr (op, val1, val2) =
  case op of
    Equals     -> show (flow1 .==. flow2) == show (val1 == val2) ++ "_<H>" &&
                  show (flow2 .==. flow1) == show (val2 == val1) ++ "_<H>" &&
                  show (flow1 .==. flow2) == show (flow2 .==. flow1)
    NotEquals  -> show (flow1 ./=. flow2) == show (val1 /= val2) ++ "_<H>" &&
                  show (flow2 ./=. flow1) == show (val2 /= val1) ++ "_<H>" &&
                  show (flow1 ./=. flow2) == show (flow2 ./=. flow1)
  where flow1 = mkHigh val1
        flow2 = mkHigh val2

prop_mixedEqExpr :: (EqOp, Bool, Bool) -> Bool
prop_mixedEqExpr (op, val1, val2) =
  case op of
    Equals     -> show (flow1 .==. flow2) == show (val1 == val2) ++ "_<H>" &&
                  show (flow2 .==. flow1) == show (val2 == val1) ++ "_<H>" &&
                  show (flow1 .==. flow2) == show (flow2 .==. flow1)
    NotEquals  -> show (flow1 ./=. flow2) == show (val1 /= val2) ++ "_<H>" &&
                  show (flow2 ./=. flow1) == show (val2 /= val1) ++ "_<H>" &&
                  show (flow1 ./=. flow2) == show (flow2 ./=. flow1)
  where flow1 = mkHigh val1
        flow2 = mkLow val2


prop_lowOrdExpr :: (OrdOp, Int, Int) -> Bool
prop_lowOrdExpr (op, val1, val2) =
  case op of
    Less           -> show (flow1 .<. flow2) == show (val1 < val2) ++ "_<L>" &&
                      show (flow2 .<. flow1) == show (val2 < val1) ++ "_<L>"
    GreaterEquals  -> show (flow1 .>=. flow2) == show (val1 >= val2) ++ "_<L>" &&
                      show (flow2 .>=. flow1) == show (val2 >= val1) ++ "_<L>"
    LessEquals     -> show (flow1 .<=. flow2) == show (val1 <= val2) ++ "_<L>" &&
                      show (flow2 .<=. flow1) == show (val2 <= val1) ++ "_<L>"
    Greater        -> show (flow1 .>. flow2) == show (val1 > val2) ++ "_<L>" &&
                      show (flow2 .>. flow1) == show (val2 > val1) ++ "_<L>"
    Max            -> show maxFlow12 == show (max val1 val2) ++ "_<L>" &&
                      show maxFlow21 == show (max val2 val1) ++ "_<L>" &&
                      show maxFlow12 == show maxFlow21
      where maxFlow12 = fMax flow1 flow2
            maxFlow21 = fMax flow2 flow1
    Min            -> show minFlow12 == show (min val1 val2) ++ "_<L>" &&
                      show minFlow21 == show (min val2 val1) ++ "_<L>" &&
                      show minFlow12 == show minFlow21
      where minFlow12 = fMin flow1 flow2
            minFlow21 = fMin flow2 flow1
  where flow1 = mkLow val1
        flow2 = mkLow val2

prop_highOrdExpr :: (OrdOp, Int, Int) -> Bool
prop_highOrdExpr (op, val1, val2) =
  case op of
    Less           -> show (flow1 .<. flow2) == show (val1 < val2) ++ "_<H>" &&
                      show (flow2 .<. flow1) == show (val2 < val1) ++ "_<H>"
    GreaterEquals  -> show (flow1 .>=. flow2) == show (val1 >= val2) ++ "_<H>" &&
                      show (flow2 .>=. flow1) == show (val2 >= val1) ++ "_<H>"
    LessEquals     -> show (flow1 .<=. flow2) == show (val1 <= val2) ++ "_<H>" &&
                      show (flow2 .<=. flow1) == show (val2 <= val1) ++ "_<H>"
    Greater        -> show (flow1 .>. flow2) == show (val1 > val2) ++ "_<H>" &&
                      show (flow2 .>. flow1) == show (val2 > val1) ++ "_<H>"
    Max            -> show maxFlow12 == show (max val1 val2) ++ "_<H>" &&
                      show maxFlow21 == show (max val2 val1) ++ "_<H>" &&
                      show maxFlow12 == show maxFlow21
      where maxFlow12 = fMax flow1 flow2
            maxFlow21 = fMax flow2 flow1
    Min            -> show minFlow12 == show (min val1 val2) ++ "_<H>" &&
                      show minFlow21 == show (min val2 val1) ++ "_<H>" &&
                      show minFlow12 == show minFlow21
      where minFlow12 = fMin flow1 flow2
            minFlow21 = fMin flow2 flow1
  where flow1 = mkHigh val1
        flow2 = mkHigh val2

prop_mixedOrdExpr :: (OrdOp, Int, Int) -> Bool
prop_mixedOrdExpr (op, val1, val2) =
  case op of
    Less           -> show (flow1 .<. flow2) == show (val1 < val2) ++ "_<H>" &&
                      show (flow2 .<. flow1) == show (val2 < val1) ++ "_<H>"
    GreaterEquals  -> show (flow1 .>=. flow2) == show (val1 >= val2) ++ "_<H>" &&
                      show (flow2 .>=. flow1) == show (val2 >= val1) ++ "_<H>"
    LessEquals     -> show (flow1 .<=. flow2) == show (val1 <= val2) ++ "_<H>" &&
                      show (flow2 .<=. flow1) == show (val2 <= val1) ++ "_<H>"
    Greater        -> show (flow1 .>. flow2) == show (val1 > val2) ++ "_<H>" &&
                      show (flow2 .>. flow1) == show (val2 > val1) ++ "_<H>"
    Max            -> show maxFlow12 == show (max val1 val2) ++ "_<H>" &&
                      show maxFlow21 == show (max val2 val1) ++ "_<H>" &&
                      show maxFlow12 == show maxFlow21
      where maxFlow12 = fMax flow1 flow2
            maxFlow21 = fMax flow2 flow1
    Min            -> show minFlow12 == show (min val1 val2) ++ "_<H>" &&
                      show minFlow21 == show (min val2 val1) ++ "_<H>" &&
                      show minFlow12 == show minFlow21
      where minFlow12 = fMin flow1 flow2
            minFlow21 = fMin flow2 flow1
  where flow1 = mkHigh val1
        flow2 = mkLow val2


------------------------------------------------------------------------------
-------------------------------- Test Program --------------------------------
------------------------------------------------------------------------------

main = runTests $ Args
    { replay = Nothing
    , maxSuccess = 100000
    , maxDiscardRatio = 100
    , maxSize = 10
    , chatty = True
    }

    where runTests :: Args -> IO ()
          runTests args =
            putStrLn "Testing FlowNum with only Low arguments:" >>
            quickCheckWith args prop_lowNumExpr >>
            putStrLn "Testing FlowNum with only High arguments:" >>
            quickCheckWith args prop_highNumExpr >>
            putStrLn "Testing FlowNum with mix of High and Low arguments:" >>
            quickCheckWith args prop_mixedNumExpr >>
            putStrLn "Testing FlowFrac with only Low arguments:" >>
            quickCheckWith args prop_lowFracExpr >>
            putStrLn "Testing FlowFrac with only High arguments:" >>
            quickCheckWith args prop_highFracExpr >>
            putStrLn "Testing FlowFrac with mix of High and Low arguments:" >>
            quickCheckWith args prop_mixedFracExpr >>
            putStrLn "Testing FlowBool with only Low arguments:" >>
            quickCheckWith args prop_lowBoolExpr >>
            putStrLn "Testing FlowBool with only High arguments:" >>
            quickCheckWith args prop_highBoolExpr >>
            putStrLn "Testing FlowBool with mix of High and Low arguments:" >>
            quickCheckWith args prop_mixedBoolExpr >>
            putStrLn "Testing FlowEqOrd (only Eq) with only Low arguments:" >>
            quickCheckWith args prop_lowEqExpr >>
            putStrLn "Testing FlowEqOrd (only Eq) with only High arguments:" >>
            quickCheckWith args prop_highEqExpr >>
            putStrLn "Testing FlowEqOrd (only Eq) with mix of High and Low arguments:" >>
            quickCheckWith args prop_mixedEqExpr >>
            putStrLn "Testing FlowEqOrd (only Ord) with only Low arguments:" >>
            quickCheckWith args prop_lowOrdExpr >>
            putStrLn "Testing FlowEqOrd (only Ord) with only High arguments:" >>
            quickCheckWith args prop_highOrdExpr >>
            putStrLn "Testing FlowEqOrd (only Ord) with mix of High and Low arguments:" >>
            quickCheckWith args prop_mixedOrdExpr
