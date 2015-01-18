{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import Haste.Foreign

-- Make it Opaque so it won't be touched or marshalled
upg :: a -> IO a
upg = fmap fromOpaque . ffi "upg" . toOpaque

lprintHaste :: Show a => a -> IO ()
lprintHaste = ffi "lprint" . show

declassify :: IO a -> IO a
declassify = fmap fromOpaque . ffi "declassify" . toOpaque
