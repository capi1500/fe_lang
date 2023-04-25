module Exec.Utils where
import Common.Ast
import Common.Types

valueOfBlock :: [Value] -> Value
valueOfBlock statements =
    if null statements then
        VUnit
    else
        last statements
