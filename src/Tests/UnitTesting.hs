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


testFlowFracDiv :: (Int, Int)
testFlowFracDiv = let flow1 = mkLow 4
                      flow2 = mkHigh 5
                      flow3 = mkLow (-4)
                      result1 = flow1 ./. flow2
                      result2 = flow2 ./. flow1
                      result3 = flow1 ./. flow3
                      result4 = flow3 ./. flow1
                      result5 = flow2 ./. flow3
                      result6 = flow3 ./. flow2
                      resHigh = validateHigh [ (result1, "0.8_<H>")
                                             , (result2, "1.25_<H>")
                                             , (result5, "-1.25_<H>")
                                             , (result6, "-0.8_<H>")
                                             ]
                      resLow = validateLow [ (result3, "-1.0_<L>")
                                           , (result4, "-1.0_<L>")
                                           ]
                      res = resHigh ++ resLow
                  in addTuples res

testFlowFracRecip :: (Int, Int)
testFlowFracRecip = let flow1 = mkLow 4
                        flow2 = mkLow (-4)
                        flow3 = mkHigh 5
                        flow4 = mkHigh (-5)
                        result1 = fRecip flow1
                        result2 = fRecip flow2
                        result3 = fRecip flow3
                        result4 = fRecip flow4
                        resHigh = validateHigh [ (result3, "0.2_<H>")
                                               , (result4, "-0.2_<H>")
                                               ]
                        resLow = validateLow [ (result1, "0.25_<L>")
                                             , (result2, "-0.25_<L>")
                                             ]
                        res = resHigh ++ resLow
                    in addTuples res


testFlowFracFromRat :: (Int, Int)
testFlowFracFromRat = let flow1 = mkLow 4
                          flow2 = mkLow (-2.5)
                          flow3 = mkHigh 5
                          flow4 = mkHigh (-3.8)
                          result1 = fFromRational flow1
                          result2 = fFromRational flow2
                          result3 = fFromRational flow3
                          result4 = fFromRational flow4
                          resHigh = validateHigh [ (result3, "5.0_<H>")
                                                 , (result4, "-3.8_<H>")
                                                 ]
                          resLow = validateLow [ (result1, "4.0_<L>")
                                               , (result2, "-2.5_<L>")
                                               ]
                          res = resHigh ++ resLow
                      in addTuples res

testFlowBoolAnd :: (Int, Int)
testFlowBoolAnd = let flow1 = mkLow True
                      flow2 = mkLow False
                      flow3 = mkHigh True
                      flow4 = mkHigh False
                      result1 = flow1 .&&. flow1
                      result2 = flow1 .&&. flow2
                      result3 = flow1 .&&. flow3
                      result4 = flow1 .&&. flow4
                      result5 = flow2 .&&. flow1
                      result6 = flow2 .&&. flow2
                      result7 = flow2 .&&. flow3
                      result8 = flow2 .&&. flow4
                      result9 = flow3 .&&. flow1
                      result10 = flow3 .&&. flow2
                      result11 = flow3 .&&. flow3
                      result12 = flow3 .&&. flow4
                      result13 = flow4 .&&. flow1
                      result14 = flow4 .&&. flow2
                      result15 = flow4 .&&. flow3
                      result16 = flow4 .&&. flow4
                      resHigh = validateHigh [ (result3, "True_<H>")
                                             , (result4, "False_<H>")
                                             , (result7, "False_<H>")
                                             , (result8, "False_<H>")
                                             , (result9, "True_<H>")
                                             , (result10, "False_<H>")
                                             , (result11, "True_<H>")
                                             , (result12, "False_<H>")
                                             , (result13, "False_<H>")
                                             , (result14, "False_<H>")
                                             , (result15, "False_<H>")
                                             , (result16, "False_<H>")
                                             ]
                      resLow = validateLow [ (result1, "True_<L>")
                                           , (result2, "False_<L>")
                                           , (result5, "False_<L>")
                                           , (result6, "False_<L>")
                                           ]
                      res = resHigh ++ resLow
                  in addTuples res

testFlowBoolOr :: (Int, Int)
testFlowBoolOr = let flow1 = mkLow True
                     flow2 = mkLow False
                     flow3 = mkHigh True
                     flow4 = mkHigh False
                     result1 = flow1 .||. flow1
                     result2 = flow1 .||. flow2
                     result3 = flow1 .||. flow3
                     result4 = flow1 .||. flow4
                     result5 = flow2 .||. flow1
                     result6 = flow2 .||. flow2
                     result7 = flow2 .||. flow3
                     result8 = flow2 .||. flow4
                     result9 = flow3 .||. flow1
                     result10 = flow3 .||. flow2
                     result11 = flow3 .||. flow3
                     result12 = flow3 .||. flow4
                     result13 = flow4 .||. flow1
                     result14 = flow4 .||. flow2
                     result15 = flow4 .||. flow3
                     result16 = flow4 .||. flow4
                     resHigh = validateHigh [ (result3, "True_<H>")
                                            , (result4, "True_<H>")
                                            , (result7, "True_<H>")
                                            , (result8, "False_<H>")
                                            , (result9, "True_<H>")
                                            , (result10, "True_<H>")
                                            , (result11, "True_<H>")
                                            , (result12, "True_<H>")
                                            , (result13, "True_<H>")
                                            , (result14, "False_<H>")
                                            , (result15, "True_<H>")
                                            , (result16, "False_<H>")
                                            ]
                     resLow = validateLow [ (result1, "True_<L>")
                                          , (result2, "True_<L>")
                                          , (result5, "True_<L>")
                                          , (result6, "False_<L>")
                                          ]
                     res = resHigh ++ resLow
                 in addTuples res


testFlowBoolNot :: (Int, Int)
testFlowBoolNot = let flow1 = mkLow True
                      flow2 = mkLow False
                      flow3 = mkHigh True
                      flow4 = mkHigh False
                      result1 = fNot flow1
                      result2 = fNot flow2
                      result3 = fNot flow3
                      result4 = fNot flow4
                      resHigh = validateHigh [ (result3, "False_<H>")
                                             , (result4, "True_<H>")
                                             ]
                      resLow = validateLow [ (result1, "False_<L>")
                                           , (result2, "True_<L>")
                                           ]
                      res = resHigh ++ resLow
                  in addTuples res

testFlowEqOrdEq :: (Int, Int)
testFlowEqOrdEq = let flow1 = mkLow 10
                      flow2 = mkLow (-2)
                      flow3 = mkHigh 42
                      flow4 = mkHigh (-12)
                      result1 = flow1 .==. flow1
                      result2 = flow1 .==. flow2
                      result3 = flow1 .==. flow3
                      result4 = flow1 .==. flow4
                      result5 = flow2 .==. flow1
                      result6 = flow2 .==. flow2
                      result7 = flow2 .==. flow3
                      result8 = flow2 .==. flow4
                      result9 = flow3 .==. flow1
                      result10 = flow3 .==. flow2
                      result11 = flow3 .==. flow3
                      result12 = flow3 .==. flow4
                      result13 = flow4 .==. flow1
                      result14 = flow4 .==. flow2
                      result15 = flow4 .==. flow3
                      result16 = flow4 .==. flow4
                      
                      resHigh = validateHigh [ (result3, "False_<H>")
                                             , (result4, "False_<H>")
                                             , (result7, "False_<H>")
                                             , (result8, "False_<H>")
                                             , (result9, "False_<H>")
                                             , (result10, "False_<H>")
                                             , (result11, "True_<H>")
                                             , (result12, "False_<H>")
                                             , (result13, "False_<H>")
                                             , (result14, "False_<H>")
                                             , (result15, "False_<H>")
                                             , (result16, "True_<H>")
                                             ]
                      resLow = validateLow [ (result1, "True_<L>")
                                           , (result2, "False_<L>")
                                           , (result5, "False_<L>")
                                           , (result6, "True_<L>")
                                           ]
                      res = resHigh ++ resLow
                  in addTuples res

testFlowEqOrdNEq :: (Int, Int)
testFlowEqOrdNEq = let flow1 = mkLow 10
                       flow2 = mkLow (-2)
                       flow3 = mkHigh 42
                       flow4 = mkHigh (-12)
                       result1 = flow1 ./=. flow1
                       result2 = flow1 ./=. flow2
                       result3 = flow1 ./=. flow3
                       result4 = flow1 ./=. flow4
                       result5 = flow2 ./=. flow1
                       result6 = flow2 ./=. flow2
                       result7 = flow2 ./=. flow3
                       result8 = flow2 ./=. flow4
                       result9 = flow3 ./=. flow1
                       result10 = flow3 ./=. flow2
                       result11 = flow3 ./=. flow3
                       result12 = flow3 ./=. flow4
                       result13 = flow4 ./=. flow1
                       result14 = flow4 ./=. flow2
                       result15 = flow4 ./=. flow3
                       result16 = flow4 ./=. flow4
                       
                       resHigh = validateHigh [ (result3, "True_<H>")
                                              , (result4, "True_<H>")
                                              , (result7, "True_<H>")
                                              , (result8, "True_<H>")
                                              , (result9, "True_<H>")
                                              , (result10, "True_<H>")
                                              , (result11, "False_<H>")
                                              , (result12, "True_<H>")
                                              , (result13, "True_<H>")
                                              , (result14, "True_<H>")
                                              , (result15, "True_<H>")
                                              , (result16, "False_<H>")
                                              ]
                       resLow = validateLow [ (result1, "False_<L>")
                                            , (result2, "True_<L>")
                                            , (result5, "True_<L>")
                                            , (result6, "False_<L>")
                                            ]
                       res = resHigh ++ resLow
                   in addTuples res

testFlowEqOrdLT :: (Int, Int)
testFlowEqOrdLT = let flow1 = mkLow 10
                      flow2 = mkLow (-2)
                      flow3 = mkHigh 42
                      flow4 = mkHigh (-12)
                      result1 = flow1 .<. flow1
                      result2 = flow1 .<. flow2
                      result3 = flow1 .<. flow3
                      result4 = flow1 .<. flow4
                      result5 = flow2 .<. flow1
                      result6 = flow2 .<. flow2
                      result7 = flow2 .<. flow3
                      result8 = flow2 .<. flow4
                      result9 = flow3 .<. flow1
                      result10 = flow3 .<. flow2
                      result11 = flow3 .<. flow3
                      result12 = flow3 .<. flow4
                      result13 = flow4 .<. flow1
                      result14 = flow4 .<. flow2
                      result15 = flow4 .<. flow3
                      result16 = flow4 .<. flow4
                      
                      resHigh = validateHigh [ (result3, "True_<H>")
                                             , (result4, "False_<H>")
                                             , (result7, "True_<H>")
                                             , (result8, "False_<H>")
                                             , (result9, "False_<H>")
                                             , (result10, "False_<H>")
                                             , (result11, "False_<H>")
                                             , (result12, "False_<H>")
                                             , (result13, "True_<H>")
                                             , (result14, "True_<H>")
                                             , (result15, "True_<H>")
                                             , (result16, "False_<H>")
                                             ]
                      resLow = validateLow [ (result1, "False_<L>")
                                           , (result2, "False_<L>")
                                           , (result5, "True_<L>")
                                           , (result6, "False_<L>")
                                           ]
                      res = resHigh ++ resLow
                  in addTuples res


testFlowEqOrdLEQ :: (Int, Int)
testFlowEqOrdLEQ = let flow1 = mkLow 10
                       flow2 = mkLow (-2)
                       flow3 = mkHigh 42
                       flow4 = mkHigh (-12)
                       result1 = flow1 .<=. flow1
                       result2 = flow1 .<=. flow2
                       result3 = flow1 .<=. flow3
                       result4 = flow1 .<=. flow4
                       result5 = flow2 .<=. flow1
                       result6 = flow2 .<=. flow2
                       result7 = flow2 .<=. flow3
                       result8 = flow2 .<=. flow4
                       result9 = flow3 .<=. flow1
                       result10 = flow3 .<=. flow2
                       result11 = flow3 .<=. flow3
                       result12 = flow3 .<=. flow4
                       result13 = flow4 .<=. flow1
                       result14 = flow4 .<=. flow2
                       result15 = flow4 .<=. flow3
                       result16 = flow4 .<=. flow4
                       
                       resHigh = validateHigh [ (result3, "True_<H>")
                                              , (result4, "False_<H>")
                                              , (result7, "True_<H>")
                                              , (result8, "False_<H>")
                                              , (result9, "False_<H>")
                                              , (result10, "False_<H>")
                                              , (result11, "True_<H>")
                                              , (result12, "False_<H>")
                                              , (result13, "True_<H>")
                                              , (result14, "True_<H>")
                                              , (result15, "True_<H>")
                                              , (result16, "True_<H>")
                                              ]
                       resLow = validateLow [ (result1, "True_<L>")
                                            , (result2, "False_<L>")
                                            , (result5, "True_<L>")
                                            , (result6, "True_<L>")
                                            ]
                       res = resHigh ++ resLow
                   in addTuples res

testFlowEqOrdGT :: (Int, Int)
testFlowEqOrdGT = let flow1 = mkLow 10
                      flow2 = mkLow (-2)
                      flow3 = mkHigh 42
                      flow4 = mkHigh (-12)
                      result1 = flow1 .>. flow1
                      result2 = flow1 .>. flow2
                      result3 = flow1 .>. flow3
                      result4 = flow1 .>. flow4
                      result5 = flow2 .>. flow1
                      result6 = flow2 .>. flow2
                      result7 = flow2 .>. flow3
                      result8 = flow2 .>. flow4
                      result9 = flow3 .>. flow1
                      result10 = flow3 .>. flow2
                      result11 = flow3 .>. flow3
                      result12 = flow3 .>. flow4
                      result13 = flow4 .>. flow1
                      result14 = flow4 .>. flow2
                      result15 = flow4 .>. flow3
                      result16 = flow4 .>. flow4

                      resHigh = validateHigh [ (result3, "False_<H>")
                                              , (result4, "True_<H>")
                                              , (result7, "False_<H>")
                                              , (result8, "True_<H>")
                                              , (result9, "True_<H>")
                                              , (result10, "True_<H>")
                                              , (result11, "False_<H>")
                                              , (result12, "True_<H>")
                                              , (result13, "False_<H>")
                                              , (result14, "False_<H>")
                                              , (result15, "False_<H>")
                                              , (result16, "False_<H>")
                                              ]
                      resLow = validateLow [ (result1, "False_<L>")
                                           , (result2, "True_<L>")
                                           , (result5, "False_<L>")
                                           , (result6, "False_<L>")
                                           ]
                      
                      res = resHigh ++ resLow
                  in addTuples res


testFlowEqOrdGEQ :: (Int, Int)
testFlowEqOrdGEQ = let flow1 = mkLow 10
                       flow2 = mkLow (-2)
                       flow3 = mkHigh 42
                       flow4 = mkHigh (-12)
                       result1 = flow1 .>=. flow1
                       result2 = flow1 .>=. flow2
                       result3 = flow1 .>=. flow3
                       result4 = flow1 .>=. flow4
                       result5 = flow2 .>=. flow1
                       result6 = flow2 .>=. flow2
                       result7 = flow2 .>=. flow3
                       result8 = flow2 .>=. flow4
                       result9 = flow3 .>=. flow1
                       result10 = flow3 .>=. flow2
                       result11 = flow3 .>=. flow3
                       result12 = flow3 .>=. flow4
                       result13 = flow4 .>=. flow1
                       result14 = flow4 .>=. flow2
                       result15 = flow4 .>=. flow3
                       result16 = flow4 .>=. flow4
                       
                       resHigh = validateHigh [ (result3, "False_<H>")
                                              , (result4, "True_<H>")
                                              , (result7, "False_<H>")
                                              , (result8, "True_<H>")
                                              , (result9, "True_<H>")
                                              , (result10, "True_<H>")
                                              , (result11, "True_<H>")
                                              , (result12, "True_<H>")
                                              , (result13, "False_<H>")
                                              , (result14, "False_<H>")
                                              , (result15, "False_<H>")
                                              , (result16, "True_<H>")
                                              ]
                       resLow = validateLow [ (result1, "True_<L>")
                                            , (result2, "True_<L>")
                                            , (result5, "False_<L>")
                                            , (result6, "True_<L>")
                                            ]
                       res = resHigh ++ resLow
                   in addTuples res

testFlowEqOrdMax :: (Int, Int)
testFlowEqOrdMax = let flow1 = mkLow 10
                       flow2 = mkLow (-2)
                       flow3 = mkHigh 42
                       flow4 = mkHigh (-12)
                       result1 = fMax flow1 flow1
                       result2 = fMax flow1 flow2
                       result3 = fMax flow1 flow3
                       result4 = fMax flow1 flow4
                       result5 = fMax flow2 flow1
                       result6 = fMax flow2 flow2
                       result7 = fMax flow2 flow3
                       result8 = fMax flow2 flow4
                       result9 = fMax flow3 flow1
                       result10 = fMax flow3 flow2
                       result11 = fMax flow3 flow3
                       result12 = fMax flow3 flow4
                       result13 = fMax flow4 flow1
                       result14 = fMax flow4 flow2
                       result15 = fMax flow4 flow3
                       result16 = fMax flow4 flow4
                       
                       resHigh = validateHigh [ (result3, "42_<H>")
                                              , (result4, "10_<H>")
                                              , (result7, "42_<H>")
                                              , (result8, "-2_<H>")
                                              , (result9, "42_<H>")
                                              , (result10, "42_<H>")
                                              , (result11, "42_<H>")
                                              , (result12, "42_<H>")
                                              , (result13, "10_<H>")
                                              , (result14, "-2_<H>")
                                              , (result15, "42_<H>")
                                              , (result16, "-12_<H>")
                                              ]
                       resLow = validateLow [ (result1, "10_<L>")
                                            , (result2, "10_<L>")
                                            , (result5, "10_<L>")
                                            , (result6, "-2_<L>")
                                            ]
                       res = resHigh ++ resLow
                   in addTuples res

testFlowEqOrdMin :: (Int, Int)
testFlowEqOrdMin = let flow1 = mkLow 10
                       flow2 = mkLow (-2)
                       flow3 = mkHigh 42
                       flow4 = mkHigh (-12)
                       result1 = fMin flow1 flow1
                       result2 = fMin flow1 flow2
                       result3 = fMin flow1 flow3
                       result4 = fMin flow1 flow4
                       result5 = fMin flow2 flow1
                       result6 = fMin flow2 flow2
                       result7 = fMin flow2 flow3
                       result8 = fMin flow2 flow4
                       result9 = fMin flow3 flow1
                       result10 = fMin flow3 flow2
                       result11 = fMin flow3 flow3
                       result12 = fMin flow3 flow4
                       result13 = fMin flow4 flow1
                       result14 = fMin flow4 flow2
                       result15 = fMin flow4 flow3
                       result16 = fMin flow4 flow4
                       
                       resHigh = validateHigh [ (result3, "10_<H>")
                                              , (result4, "-12_<H>")
                                              , (result7, "-2_<H>")
                                              , (result8, "-12_<H>")
                                              , (result9, "10_<H>")
                                              , (result10, "-2_<H>")
                                              , (result11, "42_<H>")
                                              , (result12, "-12_<H>")
                                              , (result13, "-12_<H>")
                                              , (result14, "-12_<H>")
                                              , (result15, "-12_<H>")
                                              , (result16, "-12_<H>")
                                              ]
                       resLow = validateLow [ (result1, "10_<L>")
                                            , (result2, "-2_<L>")
                                            , (result5, "-2_<L>")
                                            , (result6, "-2_<L>")
                                            ]
                       res = resHigh ++ resLow
                   in addTuples res

--------------------------------------------------------------------------------

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
  putStrLn $ "FlowNum .+. : Correct = " ++ show (fst add) ++ ", Wrong = " ++ show (snd add)
  let mul = testFlowNumMul
  putStrLn $ "FlowNum .*. : Correct = " ++ show (fst mul) ++ ", Wrong = " ++ show (snd mul)
  let sub = testFlowNumSub
  putStrLn $ "FlowNum .-. : Correct = " ++ show (fst sub) ++ ", Wrong = " ++ show (snd sub)
  let sig = testFlowNumSig
  putStrLn $ "FlowNum fSig : Correct = " ++ show (fst sig) ++ ", Wrong = " ++ show (snd sig)
  let neg = testFlowNumNeg
  putStrLn $ "FlowNum fNeg : Correct = " ++ show (fst neg) ++ ", Wrong = " ++ show (snd neg)
  let abs' = testFlowNumAbs
  putStrLn $ "FlowNum fAbs : Correct = " ++ show (fst abs') ++ ", Wrong = " ++ show (snd abs')
  let div' = testFlowFracDiv
  putStrLn $ "FlowFrac ./. : Correct = " ++ show (fst div') ++ ", Wrong = " ++ show (snd div')
  let reci = testFlowFracRecip
  putStrLn $ "FlowFrac fRecip : Correct = " ++ show (fst reci) ++ ", Wrong = " ++ show (snd reci)
  let fr = testFlowFracFromRat
  putStrLn $ "FlowFrac fFromRational : Correct = " ++ show (fst fr) ++ ", Wrong = " ++ show (snd fr)
  let and = testFlowBoolAnd
  putStrLn $ "FlowBool .&&. : Correct = " ++ show (fst and) ++ ", Wrong = " ++ show (snd and)
  let or = testFlowBoolOr
  putStrLn $ "FlowBool .||. : Correct = " ++ show (fst or) ++ ", Wrong = " ++ show (snd or)
  let not = testFlowBoolNot
  putStrLn $ "FlowBool fNot : Correct = " ++ show (fst not) ++ ", Wrong = " ++ show (snd not)
  let eq = testFlowEqOrdEq
  putStrLn $ "FlowEqOrd .==. : Correct = " ++ show (fst eq) ++ ", Wrong = " ++ show (snd eq)
  let neq = testFlowEqOrdNEq
  putStrLn $ "FlowEqOrd ./=. : Correct = " ++ show (fst neq) ++ ", Wrong = " ++ show (snd neq)
  let lt = testFlowEqOrdLT
  putStrLn $ "FlowEqOrd .<. : Correct = " ++ show (fst lt) ++ ", Wrong = " ++ show (snd lt)
  let leq = testFlowEqOrdLT
  putStrLn $ "FlowEqOrd .<=. : Correct = " ++ show (fst leq) ++ ", Wrong = " ++ show (snd leq)
  let gt = testFlowEqOrdGT
  putStrLn $ "FlowEqOrd .>. : Correct = " ++ show (fst gt) ++ ", Wrong = " ++ show (snd gt)
  let geq = testFlowEqOrdGEQ
  putStrLn $ "FlowEqOrd .>=. : Correct = " ++ show (fst geq) ++ ", Wrong = " ++ show (snd geq)
  let max = testFlowEqOrdMax
  putStrLn $ "FlowEqOrd fMax : Correct = " ++ show (fst max) ++ ", Wrong = " ++ show (snd max)
  let min = testFlowEqOrdMin
  putStrLn $ "FlowEqOrd fMin : Correct = " ++ show (fst min) ++ ", Wrong = " ++ show (snd min)
  let allTestResults = [ add, mul, sub, sig, neg, abs', div', reci, fr, and, or
                       , not, eq, neq, lt, leq, gt, geq, max, min]
  let totalTests = sum $ map (\(c, w) -> c + w) allTestResults
  let allCorrect = sum $ map fst allTestResults
  let allWrong = sum $ map snd allTestResults
  putStrLn $ "\n\nTotal tests run: " ++ show totalTests ++ "\nCorrect: " ++
    show allCorrect ++ "\nWrong: " ++ show allWrong
