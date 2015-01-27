{-# LANGUAGE OverloadedStrings #-}
module SwapIFC.Haste.CodeGen where

import Haste.Foreign

-- Make it Opaque so it won't be touched or marshalled
upg :: a -> IO a
upg = fmap fromOpaque . ffi "upg" . toOpaque

lprintHaste :: Show a => a -> IO ()
lprintHaste = ffi "lprint" . show

declassifyHaste :: IO a -> IO a
declassifyHaste = fmap fromOpaque . ffi "declassify" . toOpaque
