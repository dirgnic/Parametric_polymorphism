-- Syntax.hs
module Syntax where

type Name = String
type TypeVar = String

data Type
  = TInt
  | TBool
  | TVar TypeVar
  | TFun Type Type
  deriving (Eq, Show)

data Expr
  = IntLit Int
  | BoolLit Bool
  | Var Name
  | Let Name Expr Expr
  | If Expr Expr Expr
  | App Expr Expr
  | Lam Name Type Expr
  | TypeApp Expr Type
  deriving (Eq, Show)

data TopLevel
  = Def Name [TypeVar] [(Name, Type)] Expr
  deriving (Eq, Show)

type Program = [TopLevel]
