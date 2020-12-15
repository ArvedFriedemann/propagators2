module Data.Constraints.Types where


data Constraint = Constraint Expr Ordering Expr

data Expr
    = IntExpr Integer
    | BinopExpr BinOp Expr Expr
    | VarExpr String
  deriving stock (Eq, Ord, Show)

data BinOp
    = AddOp
    | SubOp
    | MulOp
    | DivOp
  deriving (Eq, Ord, Show)