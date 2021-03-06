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
  , newFlowRef
  , readFlowRef
  , writeFlowRef
  , modifyFlowRef
  , flowGetLine
  , unlprint
  ) where

import SwapIFC.Types
import SwapIFC.Auxiliary
import SwapIFC.Unsafe
