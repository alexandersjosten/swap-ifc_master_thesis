{-# LANGUAGE OverloadedStrings #-}
module SwapIFC.Haste.CodeGen where

import Haste.Foreign

-- Make it Opaque so it won't be touched or marshalled
upg :: a -> IO a
upg = fmap fromOpaque . ffi "upg" . toOpaque
{-
lprintHaste :: Show a => a -> IO ()
lprintHaste = p . show
  where
    {-# NOINLINE p #-}
    p :: String -> IO ()
    p = ffi "(function(x) { lprint(x); })"
-}

lprintHaste :: Show a => a -> IO ()
lprintHaste = ffi "(function(x) { lprint(x); })" . show

declassifyHaste :: IO a -> IO a
declassifyHaste ioa = do
  a <- ioa
  fmap fromOpaque $ ffi "declassify" $ toOpaque a
