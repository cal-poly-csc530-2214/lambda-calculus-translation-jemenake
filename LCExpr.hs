module LCExpr where

data Expr = 
     Num String
   | Id String
   | Abs String Expr 
   | App Expr Expr 
   | Add Expr Expr 
   | Mult Expr Expr 
   | Println Expr 
   | Ifleq0 Expr Expr Expr
   | Error String

showExpr :: Expr -> String
showExpr (Num str) = str
showExpr (Id str) = str
showExpr (Abs str exp) = "(/ " ++ str ++ " => " ++ showExpr exp ++ ")"
showExpr (App exp1 exp2) = "(" ++ showExpr exp1 ++ " " ++ showExpr exp2 ++ ")"
showExpr (Add exp1 exp2) = "(" ++ showExpr exp1 ++ " + " ++ showExpr exp2 ++ ")"
showExpr (Mult exp1 exp2) = "(" ++ showExpr exp1 ++ " * " ++ showExpr exp2 ++ ")"
showExpr (Println exp) = "println " ++ "(" ++ showExpr exp ++ ")"
showExpr (Ifleq0 exp1 exp2 exp3) = "ifleq0 " ++ showExpr exp1 ++ " " ++ showExpr exp2 ++ " " ++ showExpr exp3

showExprJS :: Expr -> String
showExprJS (Num str) = str
showExprJS (Id str) = str
showExprJS (Abs str exp) = "(" ++ str ++ ") => (" ++ showExprJS exp ++ ")"
showExprJS (App exp1 exp2) = "(" ++ showExprJS exp1 ++ ")(" ++ showExprJS exp2 ++ ")"
showExprJS (Add exp1 exp2) = "(" ++ showExprJS exp1 ++ " + " ++ showExprJS exp2 ++ ")"
showExprJS (Mult exp1 exp2) = "(" ++ showExprJS exp1 ++ " * " ++ showExprJS exp2 ++ ")"
showExprJS (Println exp) = "console.log(" ++ showExprJS exp ++ ")"
showExprJS (Ifleq0 exp1 exp2 exp3) = "(" ++ showExprJS exp1 ++ ") <=0? " ++ showExprJS exp2 ++ " : " ++ showExprJS exp3

showExprPy :: Expr -> String
showExprPy (Num str) = str
showExprPy (Id str) = str
showExprPy (Abs str exp) = "lambda " ++ str ++ ":" ++ showExprPy exp
showExprPy (App exp1 exp2) = "(" ++ showExprPy exp1 ++ ")(" ++ showExprPy exp2 ++ ")"
showExprPy (Add exp1 exp2) = "(" ++ showExprPy exp1 ++ " + " ++ showExprPy exp2 ++ ")"
showExprPy (Mult exp1 exp2) = "(" ++ showExprPy exp1 ++ " * " ++ showExprPy exp2 ++ ")"
showExprPy (Println exp) = "print(" ++ showExprPy exp ++ ")"
showExprPy (Ifleq0 exp1 exp2 exp3) = showExprPy exp2 ++ " if " ++ showExprPy exp1 ++ "<=0 else " ++ showExprPy exp3
