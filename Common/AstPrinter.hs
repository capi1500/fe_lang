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
    codePrint tabs (NewVariableStatement id (VarInitialized expr)) = printTabs tabs ++ "let " ++ id ++ " = " ++ codePrint tabs expr ++ ";\n"
    codePrint tabs (NewVariableStatement id VarUninitialized) = printTabs tabs ++ "let " ++ id ++ ";\n"
    codePrint tabs (NewFunctionStatement id expr params) = printTabs tabs ++ "fn " ++ id ++ "(" ++ intercalate ", " params ++ ") " ++ codePrint tabs expr ++ "\n"
    codePrint tabs (ExpressionStatement expr) = printTabs tabs ++ codePrint tabs expr

instance CodePrint Expression where
    codePrint tabs (BlockExpression statements) = "{\n" ++ intercalate "" (fmap (codePrint (tabs + 1)) statements) ++ printTabs tabs ++ "}\n"
    codePrint tabs (CallExpression _ function params) = codePrint tabs function ++ "(" ++ intercalate ", " (fmap (codePrint tabs) params) ++ ")"
    codePrint tabs (IfExpression condition onTrue maybeOnFalse) =
        "if (" ++ codePrint tabs condition ++ ") " ++
        codePrint tabs onTrue ++
        maybe "" (\onFalse -> "\n" ++ printTabs tabs ++ "else " ++ codePrint tabs onFalse) maybeOnFalse
    codePrint tabs (WhileExpression condition block) = "while (" ++ codePrint tabs condition ++ ") " ++ codePrint tabs block
    codePrint tabs (ForExpression ident e block) = "for (" ++ ident ++ " in " ++ codePrint tabs e ++ ") " ++ codePrint tabs block
    codePrint tabs (LiteralExpression value) = codePrint tabs value
    codePrint tabs (VariableExpression ident) = ident
    codePrint tabs (ReferenceExpression e) = "&" ++ codePrint tabs e
    codePrint tabs (DereferenceExpression e) = "*" ++ codePrint tabs e
    codePrint tabs (IndexExpression _ e1 e2) = codePrint tabs e1 ++ "[" ++ codePrint tabs e2 ++ "]"
    codePrint tabs (BoolDoubleOperatorExpression op e1 e2) = codePrint tabs e1 ++ " " ++ codePrint tabs op ++ " " ++ codePrint tabs e2
    codePrint tabs (I32DoubleOperatorExpression _ op e1 e2) = codePrint tabs e1 ++ " " ++ codePrint tabs op ++ " " ++ codePrint tabs e2
    codePrint tabs (UnaryMinusExpression e) = "-" ++ codePrint tabs e
    codePrint tabs (UnaryNegationExpression e) = "!(" ++ codePrint tabs e ++ ")"
    codePrint tabs (MakeArrayExpression values) =
        "[" ++ intercalate ", " (fmap (codePrint tabs) values) ++ "]"
    codePrint tabs (MakeArrayDefaultsExpression e1 e2) = "[" ++ codePrint tabs e1 ++ "; " ++ codePrint tabs e2 ++ "]"
    codePrint tabs (AssignmentExpression e1 e2) = codePrint tabs e1 ++ " = " ++ codePrint tabs e2
    codePrint tabs (RangeExpression e1 e2) = codePrint tabs e1 ++ ".." ++ codePrint tabs e2
    codePrint tabs BreakExpression = "break"
    codePrint tabs ContinueExpression = "continue"
    codePrint tabs (ReturnExpression e) = "return " ++ codePrint tabs e
    codePrint tabs expr = "<internal>"

instance CodePrint Value where
    codePrint _ (VI32 i) = show i
    codePrint _ (VChar c) = show c
    codePrint _ (VBool True) = "true"
    codePrint _ (VBool False) = "false"
    codePrint _ VUnit = "()"
    codePrint _ (VReference ptr) = "ptr(" ++ show ptr ++ ")"
    codePrint tabs (VVariable ptr (Variable var)) = "(ptr(" ++ show ptr ++ "), " ++ codePrint tabs var ++ ")"
    codePrint tabs (VVariable ptr Uninitialized) = "(ptr(" ++ show ptr ++ "), null)"
    codePrint tabs (VArray lst) = "[" ++ intercalate ", " (fmap (\x -> "ptr(" ++ codePrint tabs x ++ ")") lst) ++ "]"
    codePrint _ _ = "<internal>"

instance CodePrint BooleanDoubleOperator where
    codePrint _ Equals = "=="
    codePrint _ Greater = ">"
    codePrint _ Smaller = "<"
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
