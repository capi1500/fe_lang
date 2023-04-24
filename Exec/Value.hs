module Exec.Value where
import Fe.Abs (Ident)
import qualified Fe.Abs as A
import Prelude (Integer, Int, Char, Maybe, Bool (..))
import qualified Prelude as C (Eq, Ord, Show, Read)
import Data.Map (Map)
import Common.Types (Type)

data Variable = Variable Ident Type Value
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Value =
    VI32 Int |
    VChar Char |
    VBool Bool |
    VUnit |
    VStruct (Map Ident Value) |
    VVariant (Map Ident (Maybe Value)) |
    VFunction [Variable] (A.Expression' Type) | -- captures, code
    VArray Integer [Value] | -- size, values
    VReference Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- captures work this way because:
-- 1. If by move - function has ownership of this variable
-- 2. If by reference - value cannot be changed under no circumstances
-- 3. If by mutable reference - value can be changed only by calling this function. However, this function can change the value, release ownership, something else may want to lookup the value. There need to be a way of releasing ownership and (identifying and) updating the previous value

-- I also need some more information about functions:
-- If a function can only be called once (because ownership rules would break)
--
