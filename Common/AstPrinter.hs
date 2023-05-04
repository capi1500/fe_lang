module Common.AstPrinter where
import Common.Printer
import Common.Ast
import Data.List (intercalate)
import Data.Maybe

instance CodePrint Code where
    codePrint tabs (Code statements) = intercalate "" (fmap (codePrint tabs) statements)

instance CodePrint Statement where
    codePrint tabs EmptyStatement = ";\n"
    codePrint tabs (TypeStatement t) = printTabs tabs ++ show t
    codePrint tabs (NewVariableStatement id _ (VarInitialized expr)) = printTabs tabs ++ "let " ++ codePrint tabs id ++ " = " ++ codePrint tabs expr ++ ";\n"
    codePrint tabs (NewVariableStatement id _ VarUninitialized) = printTabs tabs ++ "let " ++ codePrint tabs id ++ ";\n"
    codePrint tabs (NewFunctionStatement id expr params) = printTabs tabs ++ "fn " ++ codePrint tabs id ++ "(" ++ intercalate ", " (fmap (codePrint tabs) params) ++ ") " ++ codePrint tabs expr ++ "\n"
    codePrint tabs (ExpressionStatement expr) = printTabs tabs ++ codePrint tabs expr

instance CodePrint TypedExpression where
    codePrint tabs (TypedExpression expr _ _ _) = codePrint tabs expr

instance CodePrint Expression where
    codePrint tabs (BlockExpression statements) = "{\n" ++ intercalate "" (fmap (codePrint (tabs + 1)) statements) ++ printTabs tabs ++ "\n}"
    codePrint tabs (CallExpression function params) = codePrint tabs function ++ "(" ++ intercalate ", " (fmap (\(_, _, e) -> codePrint tabs e) params) ++ ")"
    codePrint tabs (IfExpression condition onTrue maybeOnFalse) =
        "if (" ++ codePrint tabs condition ++ ") " ++
        codePrint tabs onTrue ++ 
        maybe "" (\onFalse -> "\n" ++ printTabs tabs ++ "else " ++ codePrint tabs onFalse) maybeOnFalse
    codePrint tabs (LiteralExpression value) = codePrint tabs value
    codePrint tabs (VariableExpression ident) = codePrint tabs ident
    codePrint tabs (BoolDoubleOperatorExpression op e1 e2) = codePrint tabs e1 ++ " " ++ codePrint tabs op ++ " " ++ codePrint tabs e2
    codePrint tabs (I32DoubleOperatorExpression op e1 e2) = codePrint tabs e1 ++ " " ++ codePrint tabs op ++ " " ++ codePrint tabs e2
    codePrint tabs (UnaryMinusExpression e) = "-" ++ codePrint tabs e
    codePrint tabs (MakeArrayExpression values) =
        if isString values then
            show (fmap (\(VChar c) -> c) values)
        else
            "[" ++ intercalate ", " (fmap (codePrint tabs) values) ++ "]"
    codePrint tabs (AssignmentExpression _ e1 e2) = codePrint tabs e1 ++ " = " ++ codePrint tabs e2
    codePrint tabs expr = show expr

instance CodePrint Value where
    codePrint _ (VI32 i) = show i
    codePrint _ (VChar c) = show c
    codePrint _ (VBool b) = show b
    codePrint _ VUnit = "()"
    codePrint _ x = show x

instance CodePrint BooleanDoubleOperator where
    codePrint _ Equals = "=="
    codePrint _ Greater = ">"
    codePrint _ Smaller = "<"
    codePrint _ GreaterEquals = ">="
    codePrint _ SmallerEquals = "<="
    codePrint _ LazyAnd = "&&"
    codePrint _ LazyOr = "||"

instance CodePrint NumericDoubleOperator where
    codePrint _ Plus = "+"
    codePrint _ Minus = "-"
    codePrint _ Multiply = "*"
    codePrint _ Divide = "/"
    codePrint _ Modulo = "%"
    codePrint _ LShift = "<<"
    codePrint _ RShift = ">>"
    codePrint _ BitOr = "|"
    codePrint _ BitAnd = "&"
    codePrint _ BitXor = "^"