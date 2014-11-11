module Auxiliary where

import Types

-- | upgrade takes a Flow with Low level and upgrades it
upgrade :: Flow Low a -> Flow High a
upgrade (Flow a) = Flow a

-- | creates a Flow with High level given a value
mkHigh :: a -> Flow High a
mkHigh = return

-- | creates a Flow with Low level given a value
mkLow :: a -> Flow Low a
mkLow = return

runFlow :: Flow t () -> IO ()
runFlow (Flow ioa) = ioa

runHigh :: Flow High () -> Flow Low ()
runHigh (Flow ioa) = Flow ioa
