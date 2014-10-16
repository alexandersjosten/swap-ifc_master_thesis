module Unsafe where

import Types
import System.IO.Unsafe

unsafeShow :: (Show a) => Flow t a -> String
unsafeShow (Flow ioa) = show $ unsafePerformIO ioa
