module Exec.Error where

import Fe.Abs (BNFC'Position)

data ExecutionError =
    DivisionByZero BNFC'Position |
    ShiftInvalidArgument Int |
    Other String |
    TypeCheckerFailed
  deriving (Eq, Ord, Show, Read)
