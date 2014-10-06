module Auxiliary where

import Types

-- | upgrade takes a Flow with Low level and upgrades it
upgrade :: Flow Low a -> Flow High a
upgrade (Flow a) = Flow a
