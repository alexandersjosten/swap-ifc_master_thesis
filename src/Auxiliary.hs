{-# Language CPP #-}

module Auxiliary where

import Types
import CodeGen
import Control.Exception

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
runFlow (Flow ioa) = do
  res <- try ioa
  case res of
    Left err -> let e = err :: SomeException
                in return ()
    Right () -> return ()

runHigh :: Flow High () -> Flow Low ()
runHigh (Flow ioa) = Flow ioa

lprint :: (Show a, Tag t) => a -> Flow t ()
#ifdef __HASTE__
lprint = Flow . lprintHaste
#else
lprint s = do
  tag <- tagRep
  Flow . putStrLn $ show s ++ tag
#endif
