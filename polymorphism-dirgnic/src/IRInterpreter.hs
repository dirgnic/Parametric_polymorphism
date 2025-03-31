{-- src/IRInterpreter.hs
module IRInterpreter (runLoweredProgram) where

import IntermediateRepresentation
import qualified Data.Map as Map
import Control.Monad (foldM)

-- The value environment maps variables to values
type ValueEnv = Map.Map Var Val

type FuncEnv = Map.Map Label JumpTarget

data EvalState = EvalState
  { valEnv :: ValueEnv
  , funcEnv :: FuncEnv
  }

runLoweredProgram :: Program -> Either String Val
runLoweredProgram prog =
  case lookup "main" [(l, tgt) | tgt@(JumpTarget l _ _) <- prog] of
    Just entry -> evalBlock (EvalState Map.empty funcMap) entry []
    Nothing -> Left "No 'main' block found"
  where
    funcMap = Map.fromList [(l, t) | t@(JumpTarget l _ _) <- prog]

-- Evaluate a JumpTarget block
evalBlock :: EvalState -> JumpTarget -> [Val] -> Either String Val
evalBlock (EvalState ve fe) (JumpTarget _ params (Block binds jump)) args = do
  let newEnv = Map.union (Map.fromList (zip params args)) ve
  valEnv' <- foldM (\e b -> evalBinding (EvalState e fe) b) newEnv binds
  evalJump (EvalState valEnv' fe) jump

-- Evaluate a single Binding and extend the env
evalBinding :: EvalState -> Binding -> Either String ValueEnv
evalBinding st@(EvalState env _) (Let x app) = do
  val <- evalApp st app
  return (Map.insert x val env)

-- Evaluate an App
evalApp :: EvalState -> App -> Either String Val
evalApp st@(EvalState env _) (Add v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Num (n1 + n2)
    _ -> Left "Type error in Add"

evalApp st@(EvalState env _) (And v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Boo b1), Right (Boo b2)) -> Right $ Boo (b1 && b2)
    _ -> Left "Type error in And"

evalApp st@(EvalState env _) (Sma v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Boo (n1 < n2)
    _ -> Left "Type error in Sma"

evalApp st@(EvalState env fe) (Call fname args) = do
  args' <- mapM (evalVal env) args
  case Map.lookup fname fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ fname)

-- Evaluate a Jump
evalJump :: EvalState -> Jump -> Either String Val
evalJump (EvalState ve fe) (End v) = evalVal ve v
evalJump (EvalState ve fe) (Jwa lbl args) = do
  args' <- mapM (evalVal ve) args
  case Map.lookup lbl fe of
    Just fn -> evalBlock (EvalState ve fe) fn args'
    Nothing -> Left ("Undefined function: " ++ lbl)
evalJump _ (Bwa _ _ _ _ _) = Left "Branch not supported in this interpreter"

-- Resolve a Val
evalVal :: ValueEnv -> Val -> Either String Val
evalVal _ v@(Num _) = Right v
evalVal _ v@(Boo _) = Right v
evalVal env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left ("Unbound variable: " ++ x) 
    -}

{-
-- src/IRInterpreter.hs
module IRInterpreter (runLoweredProgram) where

import IntermediateRepresentation
import qualified Data.Map as Map
import Control.Monad (foldM)

-- The value environment maps variables to values

type ValueEnv = Map.Map Var Val

type FuncEnv = Map.Map Label JumpTarget

data EvalState = EvalState
  { valEnv :: ValueEnv
  , funcEnv :: FuncEnv
  }

runLoweredProgram :: Program -> Either String Val
runLoweredProgram prog =
  case lookup "main" [(l, tgt) | tgt@(JumpTarget l _ _) <- prog] of
    Just entry -> evalBlock (EvalState Map.empty funcMap) entry []
    Nothing -> Left "No 'main' block found"
  where
    funcMap = Map.fromList [(l, t) | t@(JumpTarget l _ _) <- prog]

-- Evaluate a JumpTarget block
evalBlock :: EvalState -> JumpTarget -> [Val] -> Either String Val
evalBlock (EvalState ve fe) (JumpTarget _ params (Block binds jump)) args = do
  let newEnv = Map.union (Map.fromList (zip params args)) ve
  valEnv' <- foldM (\e b -> evalBinding (EvalState e fe) b) newEnv binds
  evalJump (EvalState valEnv' fe) jump

-- Evaluate a single Binding and extend the env
evalBinding :: EvalState -> Binding -> Either String ValueEnv
evalBinding st@(EvalState env _) (Let x app) = do
  val <- evalApp st app
  return (Map.insert x val env)

-- Evaluate an App
evalApp :: EvalState -> App -> Either String Val
evalApp (EvalState env _) (Add v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Num (n1 + n2)
    _ -> Left "Type error in Add"

evalApp (EvalState env _) (And v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Boo b1), Right (Boo b2)) -> Right $ Boo (b1 && b2)
    _ -> Left "Type error in And"

evalApp (EvalState env _) (Sma v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Boo (n1 < n2)
    _ -> Left "Type error in Sma"

evalApp st@(EvalState env fe) (Call fname args) = do
  args' <- mapM (evalVal env) args
  case Map.lookup fname fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ fname)

-- Evaluate a Jump
evalJump :: EvalState -> Jump -> Either String Val
evalJump (EvalState ve fe) (End v) = evalVal ve v
evalJump st@(EvalState ve fe) (Jwa lbl args) = do
  args' <- mapM (evalVal ve) args
  case Map.lookup lbl fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ lbl)
evalJump st@(EvalState ve fe) (Bwa cond lThen argsThen lElse argsElse) =
  case evalVal ve cond of
    Right (Boo b) -> do
      let target = if b then lThen else lElse
      let args = if b then argsThen else argsElse
      args' <- mapM (evalVal ve) args
      case Map.lookup target fe of
        Just fn -> evalBlock st fn args'
        Nothing -> Left ("Undefined function: " ++ target)
    Right _ -> Left "Condition in Bwa is not a boolean"
    Left err -> Left err

-- Resolve a Val
evalVal :: ValueEnv -> Val -> Either String Val
evalVal _ v@(Num _) = Right v
evalVal _ v@(Boo _) = Right v
evalVal env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left ("Unbound variable: " ++ x)
-}
{-
-- src/IRInterpreter.hs
module IRInterpreter (runLoweredProgram) where

import IntermediateRepresentation
import qualified Data.Map as Map
import Control.Monad (foldM)

-- The value environment maps variables to values
type ValueEnv = Map.Map Var Val
type FuncEnv = Map.Map Label JumpTarget

data EvalState = EvalState
  { valEnv :: ValueEnv
  , funcEnv :: FuncEnv
  }

runLoweredProgram :: Program -> Either String Val
runLoweredProgram prog =
  case lookup "main" [(l, tgt) | tgt@(JumpTarget l _ _) <- prog] of
    Just entry -> evalBlock (EvalState Map.empty funcMap) entry []
    Nothing -> Left "No 'main' block found"
  where
    funcMap = Map.fromList ([(l, t) | t@(JumpTarget l _ _) <- prog] ++ builtins)

    builtins =
      [ ("if", JumpTarget "if" ["cond", "then", "else"]
          (Block [] (Bwa (Var "cond") "then" [] "else" [])))
      ]

-- Evaluate a JumpTarget block
evalBlock :: EvalState -> JumpTarget -> [Val] -> Either String Val
evalBlock (EvalState ve fe) (JumpTarget _ params (Block binds jump)) args = do
  let newEnv = Map.union (Map.fromList (zip params args)) ve
  valEnv' <- foldM (\e b -> evalBinding (EvalState e fe) b) newEnv binds
  evalJump (EvalState valEnv' fe) jump

-- Evaluate a single Binding and extend the env
evalBinding :: EvalState -> Binding -> Either String ValueEnv
evalBinding st@(EvalState env _) (Let x app) = do
  val <- evalApp st app
  return (Map.insert x val env)

-- Evaluate an App
evalApp :: EvalState -> App -> Either String Val
evalApp (EvalState env _) (Add v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Num (n1 + n2)
    _ -> Left "Type error in Add"

evalApp (EvalState env _) (And v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Boo b1), Right (Boo b2)) -> Right $ Boo (b1 && b2)
    _ -> Left "Type error in And"

evalApp (EvalState env _) (Sma v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Boo (n1 < n2)
    _ -> Left "Type error in Sma"

evalApp st@(EvalState env fe) (Call fname args) = do
  args' <- mapM (evalVal env) args
  case Map.lookup fname fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ fname)

-- Evaluate a Jump
evalJump :: EvalState -> Jump -> Either String Val
evalJump (EvalState ve fe) (End v) = evalVal ve v
evalJump st@(EvalState ve fe) (Jwa lbl args) = do
  args' <- mapM (evalVal ve) args
  case Map.lookup lbl fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ lbl)
evalJump st@(EvalState ve fe) (Bwa cond lThen argsThen lElse argsElse) = do
  condVal <- evalVal ve cond
  case condVal of
    Boo b -> do
      let (target, args) = if b then (lThen, argsThen) else (lElse, argsElse)
      args' <- mapM (evalVal ve) args
      case Map.lookup target fe of
        Just fn -> evalBlock st fn args'
        Nothing -> Left ("Undefined function: " ++ target)
    _ -> Left "Condition in Bwa is not a boolean"

-- Resolve a Val
evalVal :: ValueEnv -> Val -> Either String Val
evalVal _ v@(Num _) = Right v
evalVal _ v@(Boo _) = Right v
evalVal env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left ("Unbound variable: " ++ x)
-}

{-
-- src/IRInterpreter.hs
module IRInterpreter (runLoweredProgram) where

import IntermediateRepresentation
import qualified Data.Map as Map
import Control.Monad (foldM)

-- The value environment maps variables to values
type ValueEnv = Map.Map Var Val
type FuncEnv = Map.Map Label JumpTarget

data EvalState = EvalState
  { valEnv :: ValueEnv
  , funcEnv :: FuncEnv
  }

runLoweredProgram :: Program -> Either String Val
runLoweredProgram prog =
  case lookup "main" [(l, tgt) | tgt@(JumpTarget l _ _) <- prog] of
    Just entry -> evalBlock (EvalState Map.empty funcMap) entry []
    Nothing -> Left "No 'main' block found"
  where
    funcMap = Map.fromList ([(l, t) | t@(JumpTarget l _ _) <- prog] ++ builtins)

    builtins =
      [ ("if", JumpTarget "if" ["cond", "thenBranch", "elseBranch"]
          (Block [] (Bwa (Var "cond") "thenBranch" [] "elseBranch" [])))
      ]

-- Evaluate a JumpTarget block
evalBlock :: EvalState -> JumpTarget -> [Val] -> Either String Val
evalBlock (EvalState ve fe) (JumpTarget _ params (Block binds jump)) args = do
  let newEnv = Map.union (Map.fromList (zip params args)) ve
  valEnv' <- foldM (\e b -> evalBinding (EvalState e fe) b) newEnv binds
  evalJump (EvalState valEnv' fe) jump

-- Evaluate a single Binding and extend the env
evalBinding :: EvalState -> Binding -> Either String ValueEnv
evalBinding st@(EvalState env _) (Let x app) = do
  val <- evalApp st app
  return (Map.insert x val env)

-- Evaluate an App
evalApp :: EvalState -> App -> Either String Val
evalApp (EvalState env _) (Add v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Num (n1 + n2)
    _ -> Left "Type error in Add"

evalApp (EvalState env _) (And v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Boo b1), Right (Boo b2)) -> Right $ Boo (b1 && b2)
    _ -> Left "Type error in And"

evalApp (EvalState env _) (Sma v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Boo (n1 < n2)
    _ -> Left "Type error in Sma"

evalApp st@(EvalState env fe) (Call fname args) = do
  args' <- mapM (evalVal env) args
  case Map.lookup fname fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ fname)

-- Evaluate a Jump
evalJump :: EvalState -> Jump -> Either String Val
evalJump (EvalState ve fe) (End v) = evalVal ve v
evalJump st@(EvalState ve fe) (Jwa lbl args) = do
  args' <- mapM (evalVal ve) args
  case Map.lookup lbl fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ lbl)
evalJump st@(EvalState ve fe) (Bwa cond lThen argsThen lElse argsElse) = do
  condVal <- evalVal ve cond
  case condVal of
    Boo b -> do
      let (target, args) = if b then (lThen, argsThen) else (lElse, argsElse)
      args' <- mapM (evalVal ve) args
      case Map.lookup target fe of
        Just fn -> evalBlock st fn args'
        Nothing -> Left ("Undefined function: " ++ target)
    _ -> Left "Condition in Bwa is not a boolean"

-- Resolve a Val
evalVal :: ValueEnv -> Val -> Either String Val
evalVal _ v@(Num _) = Right v
evalVal _ v@(Boo _) = Right v
evalVal env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left ("Unbound variable: " ++ x)
-}
{-
-- src/IRInterpreter.hs
module IRInterpreter (runLoweredProgram) where

import IntermediateRepresentation
import qualified Data.Map as Map
import Control.Monad (foldM)

-- The value environment maps variables to values
type ValueEnv = Map.Map Var Val
type FuncEnv = Map.Map Label JumpTarget

data EvalState = EvalState
  { valEnv :: ValueEnv
  , funcEnv :: FuncEnv
  }

runLoweredProgram :: Program -> Either String Val
runLoweredProgram prog =
  case lookup "main" [(l, tgt) | tgt@(JumpTarget l _ _) <- prog] of
    Just entry -> evalBlock (EvalState Map.empty funcMap) entry []
    Nothing -> Left "No 'main' block found"
  where
    funcMap = Map.fromList ([(l, t) | t@(JumpTarget l _ _) <- prog] ++ builtins)

    builtins =
      [ ("if", JumpTarget "if" ["cond", "x", "y"]
          (Block [] (Bwa (Var "cond") "x" [] "y" [])))
      ]

-- Evaluate a JumpTarget block
evalBlock :: EvalState -> JumpTarget -> [Val] -> Either String Val
evalBlock (EvalState ve fe) (JumpTarget _ params (Block binds jump)) args = do
  let newEnv = Map.union (Map.fromList (zip params args)) ve
  valEnv' <- foldM (\e b -> evalBinding (EvalState e fe) b) newEnv binds
  evalJump (EvalState valEnv' fe) jump

-- Evaluate a single Binding and extend the env
evalBinding :: EvalState -> Binding -> Either String ValueEnv
evalBinding st@(EvalState env _) (Let x app) = do
  val <- evalApp st app
  return (Map.insert x val env)

-- Evaluate an App
evalApp :: EvalState -> App -> Either String Val
evalApp (EvalState env _) (Add v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Num (n1 + n2)
    _ -> Left "Type error in Add"

evalApp (EvalState env _) (And v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Boo b1), Right (Boo b2)) -> Right $ Boo (b1 && b2)
    _ -> Left "Type error in And"

evalApp (EvalState env _) (Sma v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Boo (n1 < n2)
    _ -> Left "Type error in Sma"

evalApp st@(EvalState env fe) (Call fname args) = do
  args' <- mapM (evalVal env) args
  case Map.lookup fname fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ fname)

-- Evaluate a Jump
evalJump :: EvalState -> Jump -> Either String Val
evalJump (EvalState ve fe) (End v) = evalVal ve v
evalJump st@(EvalState ve fe) (Jwa lbl args) = do
  args' <- mapM (evalVal ve) args
  case Map.lookup lbl fe of
    Just fn -> evalBlock (EvalState ve fe) fn args'
    Nothing -> Left ("Undefined function: " ++ lbl)
evalJump st@(EvalState ve fe) (Bwa cond lThen argsThen lElse argsElse) = do
  condVal <- evalVal ve cond
  case condVal of
    Boo b -> do
      let (target, args) = if b then (lThen, argsThen) else (lElse, argsElse)
      args' <- mapM (evalVal ve) args
      case Map.lookup target fe of
        Just fn -> evalBlock st fn args'
        Nothing -> Left ("Undefined function: " ++ target)
    _ -> Left "Condition in Bwa is not a boolean"

-- Resolve a Val
evalVal :: ValueEnv -> Val -> Either String Val
evalVal _ v@(Num _) = Right v
evalVal _ v@(Boo _) = Right v
evalVal env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left ("Unbound variable: " ++ x)
-}

-- src/IRInterpreter.hs
{-
module IRInterpreter (runLoweredProgram) where

import IntermediateRepresentation
import qualified Data.Map as Map
import Control.Monad (foldM)

-- The value environment maps variables to values
type ValueEnv = Map.Map Var Val

type FuncEnv = Map.Map Label JumpTarget

data EvalState = EvalState
  { valEnv :: ValueEnv
  , funcEnv :: FuncEnv
  }

runLoweredProgram :: Program -> Either String Val
runLoweredProgram prog =
  case lookup "main" [(l, tgt) | tgt@(JumpTarget l _ _) <- prog] of
    Just entry -> evalBlock (EvalState Map.empty funcMap) entry []
    Nothing -> Left "No 'main' block found"
  where
    funcMap = Map.fromList [(l, t) | t@(JumpTarget l _ _) <- prog]

-- Evaluate a JumpTarget block
evalBlock :: EvalState -> JumpTarget -> [Val] -> Either String Val
evalBlock (EvalState ve fe) (JumpTarget _ params (Block binds jump)) args = do
  let newEnv = Map.union (Map.fromList (zip params args)) ve
  valEnv' <- foldM (\e b -> evalBinding (EvalState e fe) b) newEnv binds
  evalJump (EvalState valEnv' fe) jump

-- Evaluate a single Binding and extend the env
evalBinding :: EvalState -> Binding -> Either String ValueEnv
evalBinding st@(EvalState env _) (Let x app) = do
  val <- evalApp st app
  return (Map.insert x val env)

-- Evaluate an App
evalApp :: EvalState -> App -> Either String Val
evalApp st@(EvalState env _) (Add v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Num (n1 + n2)
    (Right v, _) -> Right v -- Pass-through if not actually arithmetic
    _ -> Left "Type error in Add"

evalApp st@(EvalState env _) (And v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Boo b1), Right (Boo b2)) -> Right $ Boo (b1 && b2)
    _ -> Left "Type error in And"

evalApp st@(EvalState env _) (Sma v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Boo (n1 < n2)
    _ -> Left "Type error in Sma"

evalApp st@(EvalState env fe) (Call fname args) = do
  args' <- mapM (evalVal env) args
  case Map.lookup fname fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ fname)

-- Evaluate a Jump
evalJump :: EvalState -> Jump -> Either String Val
evalJump (EvalState ve fe) (End v) = evalVal ve v
evalJump (EvalState ve fe) (Jwa lbl args) = do
  args' <- mapM (evalVal ve) args
  case Map.lookup lbl fe of
    Just fn -> evalBlock (EvalState ve fe) fn args'
    Nothing -> Left ("Undefined function: " ++ lbl)
evalJump st@(EvalState ve fe) (Bwa cond lThen argsThen lElse argsElse) =
  case evalVal ve cond of
    Right (Boo b) -> do
      let target = if b then lThen else lElse
      let args = if b then argsThen else argsElse
      args' <- mapM (evalVal ve) args
      case Map.lookup target fe of
        Just fn -> evalBlock st fn args'
        Nothing -> Left ("Undefined function: " ++ target)
    Right _ -> Left "Condition in Bwa is not a boolean"
    Left err -> Left err

-- Resolve a Val
evalVal :: ValueEnv -> Val -> Either String Val
evalVal _ v@(Num _) = Right v
evalVal _ v@(Boo _) = Right v
evalVal env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left ("Unbound variable: " ++ x)
-}

-- src/IRInterpreter.hs

module IRInterpreter (runLoweredProgram) where

import IntermediateRepresentation
import qualified Data.Map as Map
import Control.Monad (foldM)
import Debug.Trace (trace)

-- The value environment maps variables to values
type ValueEnv = Map.Map Var Val

type FuncEnv = Map.Map Label JumpTarget

data EvalState = EvalState
  { valEnv :: ValueEnv
  , funcEnv :: FuncEnv
  }

runLoweredProgram :: Program -> Either String Val
runLoweredProgram prog =
  case lookup "main" [(l, tgt) | tgt@(JumpTarget l _ _) <- prog] of
    Just entry -> evalBlock (EvalState Map.empty funcMap) entry []
    Nothing -> Left "No 'main' block found"
  where
    funcMap = Map.fromList [(l, t) | t@(JumpTarget l _ _) <- prog]

-- Evaluate a JumpTarget block
evalBlock :: EvalState -> JumpTarget -> [Val] -> Either String Val
evalBlock (EvalState ve fe) (JumpTarget _ params (Block binds jump)) args = do
  let newEnv = Map.union (Map.fromList (zip params args)) ve
  valEnv' <- foldM (\e b -> evalBinding (EvalState e fe) b) newEnv binds
  evalJump (EvalState valEnv' fe) jump

-- Evaluate a single Binding and extend the env
evalBinding :: EvalState -> Binding -> Either String ValueEnv
evalBinding st@(EvalState env _) (Let x app) = do
  val <- evalApp st app
  trace (x ++ " = " ++ show val) $ return (Map.insert x val env)
-- Evaluate an App
evalApp :: EvalState -> App -> Either String Val
evalApp st@(EvalState env _) (Add v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Num (n1 + n2)
    (Right v, _) -> Right v -- Pass-through if not actually arithmetic
    _ -> Left "Type error in Add"

evalApp st@(EvalState env _) (And v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Boo b1), Right (Boo b2)) -> Right $ Boo (b1 && b2)
    _ -> Left "Type error in And"

evalApp st@(EvalState env _) (Sma v1 v2) =
  case (evalVal env v1, evalVal env v2) of
    (Right (Num n1), Right (Num n2)) -> Right $ Boo (n1 < n2)
    _ -> Left "Type error in Sma"

evalApp st@(EvalState env fe) (Call fname args) = do
  args' <- mapM (evalVal env) args
  case Map.lookup fname fe of
    Just fn -> evalBlock st fn args'
    Nothing -> Left ("Undefined function: " ++ fname)

-- Evaluate a Jump
evalJump :: EvalState -> Jump -> Either String Val
evalJump (EvalState ve fe) (End v) = evalVal ve v
evalJump (EvalState ve fe) (Jwa lbl args) = do
  args' <- mapM (evalVal ve) args
  case Map.lookup lbl fe of
    Just fn -> evalBlock (EvalState ve fe) fn args'
    Nothing -> Left ("Undefined function: " ++ lbl)
evalJump st@(EvalState ve fe) (Bwa cond lThen argsThen lElse argsElse) =
  case evalVal ve cond of
    Right (Boo b) -> do
      let target = if b then lThen else lElse
      let args = if b then argsThen else argsElse
      args' <- mapM (evalVal ve) args
      case Map.lookup target fe of
        Just fn -> evalBlock st fn args'
        Nothing -> Left ("Undefined function: " ++ target)
    Right _ -> Left "Condition in Bwa is not a boolean"
    Left err -> Left err

-- Resolve a Val
evalVal :: ValueEnv -> Val -> Either String Val
evalVal _ v@(Num _) = Right v
evalVal _ v@(Boo _) = Right v
evalVal env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left ("Unbound variable: " ++ x)
