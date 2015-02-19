module SwapIFC (
    -- Stuff from SwapIFC.Types
    High
  , Low
  , Flow
  , FlowRef
  , FlowNum (..)
  , FlowFrac (..)
  , FlowBool (..)
  , FlowEqOrd (..)
    -- Stuff from SwapIFC.Auxiliary
  , upgrade
  , mkHigh
  , mkLow
  , runFlow
  , lprint
  , declassify
  , newFlowRef
  , readFlowRef
  , writeFlowRef
  , modifyFlowRef
  , flowGetLine
  , unlprint
    -- Stuff from SwapIFC.Unsafe
  , unsafeShow
  , unwrap
  , unwrapValue
  , changeTag
  ) where

import SwapIFC.Types
import SwapIFC.Auxiliary
import SwapIFC.Unsafe
