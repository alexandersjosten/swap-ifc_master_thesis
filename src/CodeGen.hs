{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import Haste.Foreign

-- Make it Opaque so it won't be touched or marshalled
upg :: a -> IO a
upg = fmap fromOpaque . ffi "upg" . toOpaque