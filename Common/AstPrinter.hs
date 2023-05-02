module Common.AstPrinter where
import Common.Printer
import Common.Ast
import Data.List (intercalate)
import Data.Maybe

instance CodePrint Code where
    codePrint tabs (Code statements) = intercalate "" (fmap (codePrint tabs) statements)

instance CodePrint Statement where
    codePrint tabs EmptyStatement = "\n"
    codePrint tabs (TypeStatement t) = printTabs tabs ++ show t ++ "\n"
    codePrint tabs (NewVariableStatement _ id (VarInitialized expr)) = "let " ++ codePrint tabs id ++ " = " ++ codePrint tabs expr ++ "\n"
    codePrint tabs (NewVariableStatement _ id VarUninitialized) = "let " ++ codePrint tabs id ++ "\n"
    codePrint tabs (NewFunctionStatement _ id expr params) = "fn " ++ codePrint tabs id ++ "(" ++ intercalate ", " (fmap (codePrint tabs) params) ++ ")" ++ codePrint tabs expr
    codePrint tabs (ExpressionStatement expr) = codePrint tabs expr

instance CodePrint TypedExpression where
    codePrint tabs (TypedExpression expr t l) = codePrint tabs expr

instance CodePrint Expression where
    codePrint tabs (BlockExpression statements) = "{\n" ++ printTabs (tabs + 1) ++ intercalate (printTabs (tabs + 1)) (fmap (codePrint (tabs + 1)) statements) ++ "\n" ++ printTabs tabs ++ "}\n"
    codePrint tabs (CallExpression function params) = codePrint tabs function ++ "(" ++ intercalate ", " (fmap (codePrint tabs) params) ++ ")"
    codePrint tabs (IfExpression condition onTrue maybeOnFalse) =
        "if(" ++ codePrint tabs condition ++ ")" ++
        codePrint tabs onTrue ++
        maybe "" (\onFalse -> printTabs tabs ++ "else" ++ codePrint tabs onFalse) maybeOnFalse
    codePrint tabs (LiteralExpression value) = show value
    codePrint tabs (MoveExpression expr) = "move(" ++ codePrint tabs expr ++ ")"
    codePrint tabs (VariableExpression ident) = codePrint tabs ident
    codePrint tabs (GetReferenceExpression expr) = "ref(" ++ codePrint tabs expr ++ ")"
    codePrint tabs (BoolDoubleOperatorExpression Smaller e1 e2) = codePrint tabs e1 ++ " < " ++ codePrint tabs e2
    codePrint tabs (I32DoubleOperatorExpression Plus e1 e2) = codePrint tabs e1 ++ " + " ++ codePrint tabs e2
    codePrint tabs expr = show expr
