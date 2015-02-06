module UnitTesting where

import SwapIFC
import SwapIFC.Types

testFlowNumAdd :: (Int, Int)
testFlowNumAdd = let flow1 = mkLow 10
                     flow2 = mkHigh 42
                     result1 = flow1 .+. flow2
                     result2 = flow2 .+. flow1
                     result3 = flow1 .+. flow1
                     result4 = flow2 .+. flow2
                     resHigh = validateHigh [ (result1, "52_<H>")
                                            , (result2, "52_<H>")
                                            , (result4, "84_<H>")]
                     resLow = validateLow [(result3, "20_<L>")]
                     res = resHigh ++ resLow
                 in addTuples res

testFlowNumSub :: (Int, Int)
testFlowNumSub = let flow1 = mkLow 10
                     flow2 = mkHigh 42
                     result1 = flow1 .-. flow2
                     result2 = flow2 .-. flow1
                     result3 = flow1 .-. flow1
                     result4 = flow2 .-. flow2
                     resHigh = validateHigh [ (result1, "-32_<H>")
                                            , (result2, "32_<H>")
                                            , (result4, "0_<H>")]
                     resLow = validateLow [(result3, "0_<L>")]
                     res = resHigh ++ resLow
                 in addTuples res

testFlowNumMul :: (Int, Int)
testFlowNumMul = let flow1 = mkLow 10
                     flow2 = mkHigh 42
                     result1 = flow1 .*. flow2
                     result2 = flow2 .*. flow1
                     result3 = flow1 .*. flow1
                     result4 = flow2 .*. flow2
                     resHigh = validateHigh [ (result1, "420_<H>")
                                            , (result2, "420_<H>")
                                            , (result4, "1764_<H>")]
                     resLow = validateLow [(result3, "100_<L>")]
                     res = resHigh ++ resLow
                 in addTuples res

testFlowNumSig :: (Int, Int)
testFlowNumSig = let flow1 = mkLow 10
                     flow2 = mkHigh 42
                     flow3 = mkLow (-25)
                     flow4 = mkHigh (-2)
                     result1 = fSig flow1
                     result2 = fSig flow2
                     result3 = fSig flow3
                     result4 = fSig flow4
                     resHigh = validateHigh [ (result2, "1_<H>")
                                            , (result4, "-1_<H>")]
                     resLow = validateLow [ (result1, "1_<L>")
                                          , (result3, "-1_<L>")]
                     res = resHigh ++ resLow
                 in addTuples res

testFlowNumAbs :: (Int, Int)
testFlowNumAbs = let flow1 = mkLow 10
                     flow2 = mkHigh 42
                     flow3 = mkLow (-25)
                     flow4 = mkHigh (-2)
                     result1 = fAbs flow1
                     result2 = fAbs flow2
                     result3 = fAbs flow3
                     result4 = fAbs flow4
                     resHigh = validateHigh [ (result2, "42_<H>")
                                            , (result4, "2_<H>")
                                            ]
                     resLow = validateLow [ (result1, "10_<L>"),
                                            (result3, "25_<L>")]
                     res = resHigh ++ resLow
                 in addTuples res

testFlowNumNeg :: (Int, Int)
testFlowNumNeg = let flow1 = mkLow 10
                     flow2 = mkHigh 42
                     flow3 = mkLow (-25)
                     flow4 = mkHigh (-2)
                     result1 = fNeg flow1
                     result2 = fNeg flow2
                     result3 = fNeg flow3
                     result4 = fNeg flow4
                     resHigh = validateHigh [ (result2, "-42_<H>")
                                            , (result4, "2_<H>")]
                     resLow = validateLow [ (result1, "-10_<L>")
                                          , (result3, "25_<L>")]
                     res = resHigh ++ resLow
                 in addTuples res

validateHigh :: Show a => [(Flow High a, String)] -> [(Int, Int)]
validateHigh = map (\(flow, res) -> if unsafeShow flow == res then (1, 0) else (0, 1))

validateLow :: Show a => [(Flow Low a, String)] -> [(Int, Int)]
validateLow = map (\(flow, res) -> if unsafeShow flow == res then (1, 0) else (0, 1))

addTuples :: [(Int, Int)] -> (Int, Int)
addTuples tups = addTuples' tups (0, 0)
  where addTuples' :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
        addTuples' [] acc               = acc
        addTuples' ((x, x'):xs) (y, y') = addTuples' xs (y + x, y' + x')

main = do
  let add = testFlowNumAdd
  putStrLn $ "FlowNum add: Correct = " ++ show (fst add) ++ ", Wrong = " ++ show (snd add)
  let mul = testFlowNumMul
  putStrLn $ "FlowNum mul: Correct = " ++ show (fst mul) ++ ", Wrong = " ++ show (snd mul)
  let sub = testFlowNumSub
  putStrLn $ "FlowNum sub: Correct = " ++ show (fst sub) ++ ", Wrong = " ++ show (snd sub)
  let sig = testFlowNumSig
  putStrLn $ "FlowNum signum: Correct = " ++ show (fst sig) ++ ", Wrong = " ++ show (snd sig)
  let neg = testFlowNumNeg
  putStrLn $ "FlowNum negate: Correct = " ++ show (fst neg) ++ ", Wrong = " ++ show (snd neg)
  let abs' = testFlowNumAbs
  putStrLn $ "FlowNum abs: Correct = " ++ show (fst abs') ++ ", Wrong = " ++ show (snd abs')
