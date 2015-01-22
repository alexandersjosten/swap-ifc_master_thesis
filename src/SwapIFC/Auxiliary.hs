{-# Language CPP #-}

module SwapIFC.Auxiliary where

import SwapIFC.Types
import SwapIFC.Haste.CodeGen
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

lprint :: (Show a, Tag t) => Flow t a -> Flow t ()
#ifdef __HASTE__
lprint flow = do
  a <- flow
  Flow $ lprintHaste a
#else
lprint flow = do
  s <- flow
  tag <- tagRep
  Flow . putStrLn $ show s ++ tag
#endif

run :: ValidFlow t t' => Flow t a -> Flow t' a
run = undefined

--declassify :: ValidFlow l l' => Flow l' a -> Flow l a
declassify :: Flow High a -> Flow Low a
#ifdef __HASTE__
declassify (Flow ioa) = Flow $ CodeGen.declassify ioa
#else
declassify (Flow ioa) = Flow ioa
#endif
