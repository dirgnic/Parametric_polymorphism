{-module Codegen (codegen) where

import Syntax

codegen :: Program -> String
codegen = unlines . map codegenDef

codegenDef :: TopLevel -> String
codegenDef (Def name _ args body) =
  "define " ++ name ++ "(" ++ unwords (map fst args) ++ ") {\n" ++
  indent (compile body) ++ "\n}" ++ "\n"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

compile :: Expr -> String
compile (IntLit n) = "ret i32 " ++ show n
compile (BoolLit True) = "ret i1 1"
compile (BoolLit False) = "ret i1 0"
compile (Var x) = x
compile (App f x) = compile f ++ "(" ++ compile x ++ ")"
compile (Let x e1 e2) =
  "let " ++ x ++ " = " ++ compile e1 ++ "\n" ++ compile e2
compile (If c t f) =
  "if " ++ compile c ++ " then\n" ++ indent (compile t) ++ "else\n" ++ indent (compile f)
compile (Lam x _ e) =
  "lambda " ++ x ++ " -> " ++ compile e
compile (TypeApp e _) = compile e
-}
-- src/CodeGeneration.hs
module Codegen (codegen) where
import Syntax

codegen :: Program -> String
codegen = unlines . map codegenDef

codegenDef :: TopLevel -> String
codegenDef (Def name _ args body) =
  "define " ++ name ++ "(" ++ unwords (map fst args) ++ ") {\n" ++
  indent (compile body) ++ "\n}" ++ "\n"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

compile :: Expr -> String
compile (IntLit n) = show n
compile (BoolLit True) = "true"
compile (BoolLit False) = "false"
compile (Var x) = x
compile (App f x) = compile f ++ "(" ++ compile x ++ ")"
compile (Let x e1 e2) =
  "let " ++ x ++ " = " ++ compile e1 ++ "\n" ++ compile e2
compile (If c t f) =
  "if " ++ compile c ++ " then\n" ++ indent (compile t) ++ "else\n" ++ indent (compile f)
compile (Lam x _ e) =
  "lambda " ++ x ++ " -> " ++ compile e
compile (TypeApp e _) = compile e
