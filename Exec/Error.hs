module Exec.Error where

import Fe.Abs (BNFC'Position)

data ExecutionError =
    DivisionByZero BNFC'Position |
    ShiftInvalidArgument Int |
    InputFailed |
    Other String |
    TypeCheckerFailed String
  deriving (Eq, Ord, Show, Read)
