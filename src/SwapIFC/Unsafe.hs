module SwapIFC.Unsafe where

import SwapIFC.Types
import SwapIFC.Auxiliary
import System.IO.Unsafe

unsafeShow :: (Show a, Tag t) => Flow t a -> String
unsafeShow flow = unsafePerformIO $ unwrap $ do
  tag <- tagRep
  val <- flow
  return $ show val ++ tag

unwrap :: Flow t a -> IO a
unwrap (Flow ioa) = ioa
