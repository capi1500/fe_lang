module Common.Printer where
import Data.List (intercalate)
import Fe.Abs (Ident (..))

printTabs :: Int -> String
printTabs i = intercalate "" (replicate i "    ")

class CodePrint a where
    codePrint :: Int -> a -> String

instance CodePrint Ident where
    codePrint _ (Ident value) = value

instance CodePrint a => CodePrint [a] where
    codePrint tabs lst = intercalate "," (fmap (codePrint tabs) lst)

instance CodePrint Int where
    codePrint _ i = show i
