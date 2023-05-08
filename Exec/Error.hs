module Exec.Error where

import Fe.Abs (BNFC'Position)

data ExecutionError =
    DivisionByZero BNFC'Position |
    ShiftInvalidArgument BNFC'Position Int |
    InputFailed BNFC'Position |
    IndexOutOfRange BNFC'Position Int Int | -- where, index, size
    Other String |
    TypeCheckerFailed String
  deriving (Eq, Ord, Show, Read)
