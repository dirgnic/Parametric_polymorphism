{-module Monomorphize (monomorphize) where

import Syntax
import qualified Data.Map as Map

type Env = Map.Map (Name, [Type]) Name

monomorphize :: Program -> Program
monomorphize = concatMap specializeTop

specializeTop :: TopLevel -> [TopLevel]
specializeTop (Def "main" [] args body) = [Def "main" [] args (go Map.empty body)]
  where
    go env (TypeApp (Var f) t) =
      let name' = f ++ "_" ++ show t
      in Var name'
    go env (Let x e1 e2) = Let x (go env e1) (go env e2)
    go env (If c t f) = If (go env c) (go env t) (go env f)
    go env (App f x) = App (go env f) (go env x)
    go env e = e

specializeTop (Def name [tv] [(arg, TVar v)] body)
  | v == tv =
      [ Def (name ++ "_Int") [] [(arg, TInt)] (substTypeVar tv TInt body)
      , Def (name ++ "_Bool") [] [(arg, TBool)] (substTypeVar tv TBool body)
      ]
specializeTop def = [def]

substTypeVar :: TypeVar -> Type -> Expr -> Expr
substTypeVar tv t (Lam x ty body) = Lam x (substTy ty) (substTypeVar tv t body)
  where substTy (TVar v) | v == tv = t
        substTy ty = ty
substTypeVar tv t (If a b c) = If (go a) (go b) (go c)
  where go = substTypeVar tv t
substTypeVar tv t (App a b) = App (go a) (go b)
  where go = substTypeVar tv t
substTypeVar tv t (Let x e1 e2) = Let x (go e1) (go e2)
  where go = substTypeVar tv t
substTypeVar tv t (TypeApp e ty) = TypeApp (substTypeVar tv t e) ty
substTypeVar _ _ e = e -}

-- src/Monomorphize.hs
{-
module Monomorphize (monomorphizeProgram) where

import Syntax
import Data.List (nub)

-- | Generate concrete monomorphic copies of polymorphic functions
monomorphizeProgram :: Program -> Program
monomorphizeProgram prog = concatMap specializeCallSite callSites ++ baseDefs
  where
    baseDefs = filter (not . isPoly) prog
    callSites = [ (name, tys) | Def _ _ _ body <- prog, (name, tys) <- collectTypeApps body ]

-- | Identify if a function is polymorphic
isPoly :: TopLevel -> Bool
isPoly (Def _ tyvars _ _) = not (null tyvars)

-- | Collect all TypeApp sites like f[Int], f[Bool], etc.
collectTypeApps :: Expr -> [(Name, [Type])]
collectTypeApps (TypeApp (Var f) ty) = [(f, [ty])]
collectTypeApps (App e1 e2) = collectTypeApps e1 ++ collectTypeApps e2
collectTypeApps (Let _ e1 e2) = collectTypeApps e1 ++ collectTypeApps e2
collectTypeApps (If c t f) = collectTypeApps c ++ collectTypeApps t ++ collectTypeApps f
collectTypeApps _ = []

-- | Generate monomorphic versions of polymorphic functions
specializeCallSite :: (Name, [Type]) -> [TopLevel]
specializeCallSite (fname, [ty]) =
  case lookupDef fname of
    Just (Def _ [tv] args body) ->
      let subst = [(tv, ty)]
          args' = [ (x, substTy t subst) | (x, t) <- args ]
          body' = substExpr body subst
          newName = fname ++ "_" ++ tySuffix ty
       in [Def newName [] args' body']
    _ -> []
specializeCallSite _ = []

-- | Dummy lookup for now; assumes source program has defs in scope
lookupDef :: Name -> Maybe TopLevel
lookupDef name = Nothing -- Will be injected in main pipeline with full env

-- | Substitute a type variable with a concrete type
substTy :: Type -> [(TypeVar, Type)] -> Type
substTy (TVar tv) s = maybe (TVar tv) id (lookup tv s)
substTy t _ = t

-- | Substitute within an expression
substExpr :: Expr -> [(TypeVar, Type)] -> Expr
substExpr (Var x) _ = Var x
substExpr (IntLit n) _ = IntLit n
substExpr (BoolLit b) _ = BoolLit b
substExpr (App e1 e2) s = App (substExpr e1 s) (substExpr e2 s)
substExpr (Let x e1 e2) s = Let x (substExpr e1 s) (substExpr e2 s)
substExpr (If c t f) s = If (substExpr c s) (substExpr t s) (substExpr f s)
substExpr (TypeApp e ty) s = TypeApp (substExpr e s) (substTy ty s)
substExpr (Lam x ty e) s = Lam x (substTy ty s) (substExpr e s)

-- | Encode type in name
tySuffix :: Type -> String
tySuffix TInt = "Int"
tySuffix TBool = "Bool"
tySuffix (TVar x) = x

-}

-- src/Monomorphize.hs
{-
module Monomorphize (monomorphizeProgram) where

import Syntax
import Data.List (nub)
import qualified Data.Map as Map

type Env = Map.Map Name TopLevel

-- | Generate concrete monomorphic copies of polymorphic functions
monomorphizeProgram :: Program -> Program
monomorphizeProgram prog =
  let env = Map.fromList [(name, d) | d@(Def name _ _ _) <- prog]
      callSites = [ (name, tys) | Def _ _ _ body <- prog, (name, tys) <- collectTypeApps body ]
      monoDefs = concatMap (specializeCallSite env) callSites
      baseDefs = filter (not . isPoly) prog
  in nub (baseDefs ++ monoDefs)

-- | Identify if a function is polymorphic
isPoly :: TopLevel -> Bool
isPoly (Def _ tyvars _ _) = not (null tyvars)

-- | Collect all TypeApp sites like f[Int], f[Bool], etc.
collectTypeApps :: Expr -> [(Name, [Type])]
collectTypeApps (TypeApp (Var f) ty) = [(f, [ty])]
collectTypeApps (App e1 e2) = collectTypeApps e1 ++ collectTypeApps e2
collectTypeApps (Let _ e1 e2) = collectTypeApps e1 ++ collectTypeApps e2
collectTypeApps (If c t f) = collectTypeApps c ++ collectTypeApps t ++ collectTypeApps f
collectTypeApps _ = []

-- | Generate monomorphic versions of polymorphic functions
specializeCallSite :: Env -> (Name, [Type]) -> [TopLevel]
specializeCallSite env (fname, [ty]) =
  case Map.lookup fname env of
    Just (Def _ [tv] args body) ->
      let subst = [(tv, ty)]
          args' = [ (x, substTy t subst) | (x, t) <- args ]
          body' = substExpr body subst
          newName = fname ++ "_" ++ tySuffix ty
       in [Def newName [] args' body']
    _ -> []
specializeCallSite _ _ = []

-- | Substitute a type variable with a concrete type
substTy :: Type -> [(TypeVar, Type)] -> Type
substTy (TVar tv) s = maybe (TVar tv) id (lookup tv s)
substTy t _ = t

-- | Substitute within an expression
substExpr :: Expr -> [(TypeVar, Type)] -> Expr
substExpr (Var x) _ = Var x
substExpr (IntLit n) _ = IntLit n
substExpr (BoolLit b) _ = BoolLit b
substExpr (App e1 e2) s = App (substExpr e1 s) (substExpr e2 s)
substExpr (Let x e1 e2) s = Let x (substExpr e1 s) (substExpr e2 s)
substExpr (If c t f) s = If (substExpr c s) (substExpr t s) (substExpr f s)
substExpr (TypeApp e ty) s = TypeApp (substExpr e s) (substTy ty s)
substExpr (Lam x ty e) s = Lam x (substTy ty s) (substExpr e s)

-- | Encode type in name
tySuffix :: Type -> String
tySuffix TInt = "TInt"
tySuffix TBool = "TBool"
tySuffix (TVar x) = x
-}

-- src/Monomorphize.hs
-- src/Monomorphize.hs
module Monomorphize (monomorphizeProgram) where

import Syntax
import Data.List (nub)
import qualified Data.Map as Map

type Env = Map.Map Name TopLevel

-- | Generate concrete monomorphic copies of polymorphic functions
monomorphizeProgram :: Program -> Program
monomorphizeProgram prog =
  let env = Map.fromList [(name, d) | d@(Def name _ _ _) <- prog]
      callSites = [ (name, tys) | Def _ _ _ body <- prog, (name, tys) <- collectTypeApps body ]
      monoDefs = concatMap (specializeCallSite env) callSites
      baseDefs = filter (not . isPoly) prog
      updatedDefs = map (rewriteReturns . replaceTypeAppsInBody callSites) baseDefs
  in nub (updatedDefs ++ map rewriteReturns monoDefs)


-- | Identify if a function is polymorphic
isPoly :: TopLevel -> Bool
isPoly (Def _ tyvars _ _) = not (null tyvars)

-- | Collect all TypeApp sites like f[Int], f[Bool], etc.
collectTypeApps :: Expr -> [(Name, [Type])]
collectTypeApps (TypeApp (Var f) ty) = [(f, [ty])]
collectTypeApps (App e1 e2) = collectTypeApps e1 ++ collectTypeApps e2
collectTypeApps (Let _ e1 e2) = collectTypeApps e1 ++ collectTypeApps e2
collectTypeApps (If c t f) = collectTypeApps c ++ collectTypeApps t ++ collectTypeApps f
collectTypeApps _ = []

-- | Generate monomorphic versions of polymorphic functions
specializeCallSite :: Env -> (Name, [Type]) -> [TopLevel]
specializeCallSite env (fname, [ty]) =
  case Map.lookup fname env of
    Just (Def _ [tv] args body) ->
      let subst = [(tv, ty)]
          args' = [ (x, substTy t subst) | (x, t) <- args ]
          body' = substExpr body subst
          newName = fname ++ "_" ++ tySuffix ty
       in [Def newName [] args' body']
    _ -> []
specializeCallSite _ _ = []

-- | Apply type apps as direct function renames
replaceTypeAppsInBody :: [(Name, [Type])] -> TopLevel -> TopLevel
replaceTypeAppsInBody apps (Def name tyvars args body) =
  let body' = foldr replace body apps
  in Def name tyvars args body'
  where
    replace (f, [ty]) = renameTypeApp f ty
    replace _ = id

-- | Rewrite (App (Var "return") x) => x (recursively)
rewriteReturns :: TopLevel -> TopLevel
rewriteReturns (Def name tvs args body) = Def name tvs args (go body)
  where
    go (App (Var "return") x) = go x
    go (App e1 e2) = case go e1 of
                        Var "return" -> go e2
                        newE1 -> App newE1 (go e2)
    go (Let x e1 e2) = Let x (go e1) (go e2)
    go (If c t f) = If (go c) (go t) (go f)
    go (Lam x t e) = Lam x t (go e)
    go other = other

-- | Replace TypeApp(f, ty) with Var "f_ty"
renameTypeApp :: Name -> Type -> Expr -> Expr
renameTypeApp f ty (TypeApp (Var g) t) | f == g && t == ty = Var (f ++ "_" ++ tySuffix ty)
renameTypeApp f ty (App e1 e2) = App (renameTypeApp f ty e1) (renameTypeApp f ty e2)
renameTypeApp f ty (Let x e1 e2) = Let x (renameTypeApp f ty e1) (renameTypeApp f ty e2)
renameTypeApp f ty (If c t e) = If (renameTypeApp f ty c) (renameTypeApp f ty t) (renameTypeApp f ty e)
renameTypeApp f ty (Lam x t e) = Lam x t (renameTypeApp f ty e)
renameTypeApp _ _ e = e

-- | Substitute a type variable with a concrete type
substTy :: Type -> [(TypeVar, Type)] -> Type
substTy (TVar tv) s = maybe (TVar tv) id (lookup tv s)
substTy t _ = t

-- | Substitute within an expression
substExpr :: Expr -> [(TypeVar, Type)] -> Expr
substExpr (Var x) _ = Var x
substExpr (IntLit n) _ = IntLit n
substExpr (BoolLit b) _ = BoolLit b
substExpr (App e1 e2) s = App (substExpr e1 s) (substExpr e2 s)
substExpr (Let x e1 e2) s = Let x (substExpr e1 s) (substExpr e2 s)
substExpr (If c t f) s = If (substExpr c s) (substExpr t s) (substExpr f s)
substExpr (TypeApp e ty) s = TypeApp (substExpr e s) (substTy ty s)
substExpr (Lam x ty e) s = Lam x (substTy ty s) (substExpr e s)

-- | Encode type in name
tySuffix :: Type -> String
tySuffix TInt = "TInt"
tySuffix TBool = "TBool"
tySuffix (TVar x) = x