module HuttonsRazor where
import Data.List
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit val) = val
eval (Add e e') = eval e + eval e'

printExpr :: Expr -> String
printExpr (Lit val) = show val
printExpr (Add e e') = intercalate " + " $ map printExpr [e, e']
-- printExpr (Add e e') = concat [printExpr e, " + ", printExpr e']
