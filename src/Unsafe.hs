module Unsafe where

import Types
import Auxiliary
import System.IO.Unsafe

unsafeShow :: (Show a) => Flow t a -> String
unsafeShow (Flow ioa) = show $ unsafePerformIO ioa

unwrap :: Flow t a -> IO a
unwrap (Flow ioa) = ioa
