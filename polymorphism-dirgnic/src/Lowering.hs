{-
- src/Lowering.hs
module Lowering (lowerProgram) where

import Syntax
import qualified IntermediateRepresentation as IR
import Control.Monad.State

-- Lowering monad dith unique counter
type LowerM = State Int

freshLabel :: LowerM IR.Label
freshLabel = do
  n <- get
  put (n + 1)
  return ("label" ++ show n)

freshVar :: LowerM IR.Var
freshVar = do
  n <- get
  put (n + 1)
  return ("tmp" ++ show n)

-- Entry point: convert top-level program into IR
lowerProgram :: [TopLevel] -> IR.Program
lowerProgram tops = concatMap lowerTop tops

-- Lower a single top-level function
lowerTop :: TopLevel -> [IR.JumpTarget]
lowerTop (Def name _ args body) =
  let argNames = map fst args
      (block, _) = runState (lowerExpr body >>= endBlock) 0
  in [IR.JumpTarget name argNames block]

-- Lower expressions to IR blocks
lowerExpr :: Expr -> LowerM ([IR.Binding], IR.Val)
lowerExpr (IntLit n) = return ([], IR.Num (fromIntegral n))
lowerExpr (BoolLit b) = return ([], IR.Boo b)
lowerExpr (Var x) = return ([], IR.Var x)
lowerExpr (TypeApp e _) = lowerExpr e  -- drop types during lowering
lowerExpr e@(App _ _) = lowerApp e
  where
    flatten :: Expr -> [Expr]
    flatten (App f x) = flatten f ++ [x]
    flatten other = [other]

    tySuffix :: Type -> String
    tySuffix TInt = "TInt"
    tySuffix TBool = "TBool"
    tySuffix (TVar tv) = tv

    extractFnName :: Expr -> Maybe String
    extractFnName (Var f) = Just f
    extractFnName (TypeApp (Var f) ty) = Just (f ++ "_" ++ tySuffix ty)
    extractFnName _ = Nothing

    lowerApp :: Expr -> LowerM ([IR.Binding], IR.Val)
    lowerApp expr = do
      let exprs = flatten expr
      case exprs of
        (headExpr : argEs) -> case extractFnName headExpr of
          Just f -> do
            lowered <- mapM lowerExpr argEs
            let bs = concatMap fst lowered
            let vs = map snd lowered
            tmp <- freshVar
            let call = IR.Call f vs
            return (bs ++ [IR.Let tmp call], IR.Var tmp)
          Nothing -> error "Unsupported function head"
        _ -> error "Empty application expression"

lowerExpr (Let x e1 e2) = do
  (b1, v1) <- lowerExpr e1
  let bind = IR.Let x (IR.Add v1 v1) -- placeholder binding
  (b2, v2) <- lowerExpr e2
  return (b1 ++ [bind] ++ b2, v2)

lowerExpr (If c t f) = do
  (cb, _) <- lowerExpr c
  (tb, tv) <- lowerExpr t
  (fb, fv) <- lowerExpr f
  _ <- freshLabel
  _ <- freshLabel
  _ <- freshLabel
  return (cb ++ tb ++ fb, tv) -- simplified

lowerExpr _ = error "Unsupported expr in lowering"

-- End a block with a value
endBlock :: ([IR.Binding], IR.Val) -> LowerM IR.Block
endBlock (bs, v) = return (IR.Block bs (IR.End v))

-}

-- src/Lowering.hs
module Lowering (lowerProgram) where

import Syntax
import qualified IntermediateRepresentation as IR
import Control.Monad.State

-- Lowering monad with unique counter
type LowerM = State Int

freshLabel :: LowerM IR.Label
freshLabel = do
  n <- get
  put (n + 1)
  return ("label" ++ show n)

freshVar :: LowerM IR.Var
freshVar = do
  n <- get
  put (n + 1)
  return ("tmp" ++ show n)

-- Entry point: convert top-level program into IR
lowerProgram :: [TopLevel] -> IR.Program
lowerProgram tops = concatMap lowerTop tops

-- Lower a single top-level function
lowerTop :: TopLevel -> [IR.JumpTarget]
lowerTop (Def name _ args body) =
  let argNames = map fst args
      (block, _) = runState (lowerExpr body >>= endBlock) 0
  in [IR.JumpTarget name argNames block]

-- Lower expressions to IR blocks
lowerExpr :: Expr -> LowerM ([IR.Binding], IR.Val)
lowerExpr (IntLit n) = return ([], IR.Num (fromIntegral n))
lowerExpr (BoolLit b) = return ([], IR.Boo b)
lowerExpr (Var x) = return ([], IR.Var x)
lowerExpr (TypeApp e _) = lowerExpr e  -- drop types during lowering
lowerExpr e@(App _ _) = lowerApp e
  where
    flatten :: Expr -> [Expr]
    flatten (App f x) = flatten f ++ [x]
    flatten other = [other]

    tySuffix :: Type -> String
    tySuffix TInt = "TInt"
    tySuffix TBool = "TBool"
    tySuffix (TVar tv) = tv

    extractFnName :: Expr -> Maybe String
    extractFnName (Var f) = Just f
    extractFnName (TypeApp (Var f) ty) = Just (f ++ "_" ++ tySuffix ty)
    extractFnName _ = Nothing

    lowerApp :: Expr -> LowerM ([IR.Binding], IR.Val)
    lowerApp expr = do
      let exprs = flatten expr
      case exprs of
        (headExpr : argEs) -> case extractFnName headExpr of
          Just f -> do
            lowered <- mapM lowerExpr argEs
            let bs = concatMap fst lowered
            let vs = map snd lowered
            tmp <- freshVar
            let call = IR.Call f vs
            return (bs ++ [IR.Let tmp call], IR.Var tmp)
          Nothing -> error "Unsupported function head"
        _ -> error "Empty application expression"

lowerExpr (Let x e1 e2) = do
  (b1, v1) <- lowerExpr e1
  tmp <- freshVar
  let bind = IR.Let tmp (IR.Add v1 v1) -- fake binding
  (b2, v2) <- lowerExpr e2
  return (b1 ++ [bind] ++ b2, v2)

lowerExpr (If c t f) = do
  (cb, cv) <- lowerExpr c
  (tb, tv) <- lowerExpr t
  (fb, fv) <- lowerExpr f
  lThen <- freshLabel
  lElse <- freshLabel
  lCont <- freshLabel
  tmp <- freshVar

  let thenBlock = IR.Block tb (IR.Jwa lCont [tv])
      elseBlock = IR.Block fb (IR.Jwa lCont [fv])
      condJump  = IR.Bwa cv lThen [] lElse []

  let branchBlock = cb ++ [IR.Let tmp (IR.Add cv cv)]

  return (branchBlock, IR.Var tmp)

lowerExpr _ = error "Unsupported expr in lowering"

-- End a block with a value
endBlock :: ([IR.Binding], IR.Val) -> LowerM IR.Block
endBlock (bs, v) = return (IR.Block bs (IR.End v))
