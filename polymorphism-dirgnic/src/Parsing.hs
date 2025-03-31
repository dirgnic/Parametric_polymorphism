{-'
-- src/Parsing.hs
module Parsing (parseProgram) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

-- Parser type

newtype Parser a = Parser { runParser :: Parsec Void String a }

type MParser = Parsec Void String

-- Lexer
sc :: MParser ()
sc = L.space space1 empty empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

identifier :: MParser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

reserved :: String -> MParser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

parens :: MParser a -> MParser a
parens = between (symbol "(") (symbol ")")

braces :: MParser a -> MParser a
braces = between (symbol "{") (symbol "}")

angles :: MParser a -> MParser a
angles = between (symbol "[") (symbol "]")

commaSep :: MParser a -> MParser [a]
commaSep p = p `sepBy` symbol ","

-- Types
ptype :: MParser Type
ptype = (TInt <$ reserved "Int")
    <|> (TBool <$ reserved "Bool")
    <|> (TVar <$> identifier)

-- Expressions
pexpr :: MParser Expr
pexpr = makeExprParser pterm []

pterm :: MParser Expr
pterm = choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , Var <$> identifier
  , try (do
        f <- identifier
        ty <- angles ptype
        args <- parens (commaSep pexpr)
        return (foldl App (TypeApp (Var f) ty) args))
  , parens pexpr
  ]

-- Let/If
pLet :: MParser Expr
pLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- pexpr
  symbol ";"
  e2 <- pexpr
  return (Let x e1 e2)

pIf :: MParser Expr
pIf = do
  reserved "if"
  c <- pexpr
  reserved "then"
  t <- pexpr
  reserved "else"
  f <- pexpr
  return (If c t f)

-- Top-level function
pDef :: MParser TopLevel
pDef = do
  reserved "def"
  name <- identifier
  tyvars <- option [] (angles (commaSep identifier))
  args <- parens (commaSep ((,) <$> identifier <*> (symbol ":" *> ptype)))
  body <- braces (reserved "return" *> pexpr <* symbol ";")
  return (Def name tyvars args body)

-- Program
pProgram :: MParser Program
pProgram = many (sc *> pDef <* sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram "source"
-}

{-
-- src/Parsing.hs
-- src/Parsing.hs
-- src/Parsing.hs
module Parsing (parseProgram) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

type MParser = Parsec Void String

-- Lexer
sc :: MParser ()
sc = L.space space1 empty empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

identifier :: MParser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

reserved :: String -> MParser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

parens :: MParser a -> MParser a
parens = between (symbol "(") (symbol ")")

braces :: MParser a -> MParser a
braces = between (symbol "{") (symbol "}")

angles :: MParser a -> MParser a
angles = between (symbol "[") (symbol "]")

commaSep :: MParser a -> MParser [a]
commaSep p = p `sepBy` symbol ","

-- Types
ptype :: MParser Type
ptype = (TInt <$ reserved "Int")
    <|> (TBool <$ reserved "Bool")
    <|> (TVar <$> identifier)

-- Expressions
pexpr :: MParser Expr
pexpr = choice
  [ pLet
  , try pIf
  , do es <- some pterm
       return (foldl1 App es)
  ]

pterm :: MParser Expr
pterm = choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , try (do
        f <- identifier
        ty <- angles ptype
        args <- parens (commaSep pexpr)
        return (foldl App (TypeApp (Var f) ty) args))
  , Var <$> identifier
  , parens pexpr
  ]

-- Let/If
pLet :: MParser Expr
pLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- pexpr
  optional (symbol ";")
  e2 <- pexpr
  return (Let x e1 e2)

pIf :: MParser Expr
pIf = do
  reserved "if"
  c <- pexpr
  reserved "then"
  t <- pexpr <* optional (symbol ";")
  reserved "else"
  f <- pexpr <* optional (symbol ";")
  return (If c t f)

-- Top-level function
pDef :: MParser TopLevel
pDef = do
  reserved "def"
  name <- identifier
  tyvars <- option [] (angles (commaSep identifier))
  args <- parens (commaSep ((,) <$> identifier <*> (symbol ":" *> ptype)))
  body <- braces (pexpr <* optional (symbol ";"))
  return (Def name tyvars args body)

-- Program
pProgram :: MParser Program
pProgram = many (sc *> pDef <* sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram "source"
-}

{- src/Parsing.hs
module Parsing (parseProgram) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

type MParser = Parsec Void String

-- Lexer
sc :: MParser ()
sc = L.space space1 empty empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

identifier :: MParser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

reserved :: String -> MParser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

parens :: MParser a -> MParser a
parens = between (symbol "(") (symbol ")")

braces :: MParser a -> MParser a
braces = between (symbol "{") (symbol "}")

angles :: MParser a -> MParser a
angles = between (symbol "[") (symbol "]")

commaSep :: MParser a -> MParser [a]
commaSep p = p `sepBy` symbol ","

-- Types
ptype :: MParser Type
ptype = (TInt <$ reserved "Int")
    <|> (TBool <$ reserved "Bool")
    <|> (TVar <$> identifier)

-- Expressions
pexpr :: MParser Expr
pexpr = choice
  [ pLet
  , try pIf
  , do es <- some pterm
       return (foldl1 App es)
  ]

pterm :: MParser Expr
pterm = choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , try (do
        f <- identifier
        ty <- angles ptype
        args <- parens (commaSep pexpr)
        return (foldl App (TypeApp (Var f) ty) args))
  , Var <$> identifier
  , parens pexpr
  ]

-- Let/If
pLet :: MParser Expr
pLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- pexpr
  optional (symbol ";")
  e2 <- pexpr
  return (Let x e1 e2)

pIf :: MParser Expr
pIf = do
  reserved "if"
  c <- pexpr
  reserved "then"
  t <- pexpr
  reserved "else"
  f <- pexpr
  optional (symbol ";")
  return (If c t f)

-- Top-level function
pDef :: MParser TopLevel
pDef = do
  reserved "def"
  name <- identifier
  tyvars <- option [] (angles (commaSep identifier))
  args <- parens (commaSep ((,) <$> identifier <*> (symbol ":" *> ptype)))
  body <- braces (pexpr <* optional (symbol ";"))
  return (Def name tyvars args body)

-- Program
pProgram :: MParser Program
pProgram = many (sc *> pDef <* sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram "source"
{-'
-- src/Parsing.hs
module Parsing (parseProgram) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

-- Parser type

newtype Parser a = Parser { runParser :: Parsec Void String a }

type MParser = Parsec Void String

-- Lexer
sc :: MParser ()
sc = L.space space1 empty empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

identifier :: MParser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

reserved :: String -> MParser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

parens :: MParser a -> MParser a
parens = between (symbol "(") (symbol ")")

braces :: MParser a -> MParser a
braces = between (symbol "{") (symbol "}")

angles :: MParser a -> MParser a
angles = between (symbol "[") (symbol "]")

commaSep :: MParser a -> MParser [a]
commaSep p = p `sepBy` symbol ","

-- Types
ptype :: MParser Type
ptype = (TInt <$ reserved "Int")
    <|> (TBool <$ reserved "Bool")
    <|> (TVar <$> identifier)

-- Expressions
pexpr :: MParser Expr
pexpr = makeExprParser pterm []

pterm :: MParser Expr
pterm = choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , Var <$> identifier
  , try (do
        f <- identifier
        ty <- angles ptype
        args <- parens (commaSep pexpr)
        return (foldl App (TypeApp (Var f) ty) args))
  , parens pexpr
  ]

-- Let/If
pLet :: MParser Expr
pLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- pexpr
  symbol ";"
  e2 <- pexpr
  return (Let x e1 e2)

pIf :: MParser Expr
pIf = do
  reserved "if"
  c <- pexpr
  reserved "then"
  t <- pexpr
  reserved "else"
  f <- pexpr
  return (If c t f)

-- Top-level function
pDef :: MParser TopLevel
pDef = do
  reserved "def"
  name <- identifier
  tyvars <- option [] (angles (commaSep identifier))
  args <- parens (commaSep ((,) <$> identifier <*> (symbol ":" *> ptype)))
  body <- braces (reserved "return" *> pexpr <* symbol ";")
  return (Def name tyvars args body)

-- Program
pProgram :: MParser Program
pProgram = many (sc *> pDef <* sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram "source"
-}

{-
-- src/Parsing.hs
-- src/Parsing.hs
-- src/Parsing.hs
module Parsing (parseProgram) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

type MParser = Parsec Void String

-- Lexer
sc :: MParser ()
sc = L.space space1 empty empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

identifier :: MParser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

reserved :: String -> MParser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

parens :: MParser a -> MParser a
parens = between (symbol "(") (symbol ")")

braces :: MParser a -> MParser a
braces = between (symbol "{") (symbol "}")

angles :: MParser a -> MParser a
angles = between (symbol "[") (symbol "]")

commaSep :: MParser a -> MParser [a]
commaSep p = p `sepBy` symbol ","

-- Types
ptype :: MParser Type
ptype = (TInt <$ reserved "Int")
    <|> (TBool <$ reserved "Bool")
    <|> (TVar <$> identifier)

-- Expressions
pexpr :: MParser Expr
pexpr = choice
  [ pLet
  , try pIf
  , do es <- some pterm
       return (foldl1 App es)
  ]

pterm :: MParser Expr
pterm = choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , try (do
        f <- identifier
        ty <- angles ptype
        args <- parens (commaSep pexpr)
        return (foldl App (TypeApp (Var f) ty) args))
  , Var <$> identifier
  , parens pexpr
  ]

-- Let/If
pLet :: MParser Expr
pLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- pexpr
  optional (symbol ";")
  e2 <- pexpr
  return (Let x e1 e2)

pIf :: MParser Expr
pIf = do
  reserved "if"
  c <- pexpr
  reserved "then"
  t <- pexpr <* optional (symbol ";")
  reserved "else"
  f <- pexpr <* optional (symbol ";")
  return (If c t f)

-- Top-level function
pDef :: MParser TopLevel
pDef = do
  reserved "def"
  name <- identifier
  tyvars <- option [] (angles (commaSep identifier))
  args <- parens (commaSep ((,) <$> identifier <*> (symbol ":" *> ptype)))
  body <- braces (pexpr <* optional (symbol ";"))
  return (Def name tyvars args body)

-- Program
pProgram :: MParser Program
pProgram = many (sc *> pDef <* sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram "source"
-}
-}
-- src/Parsing.hs
{-
module Parsing (parseProgram) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

type MParser = Parsec Void String

-- Lexer
sc :: MParser ()
sc = L.space space1 empty empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

identifier :: MParser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

reserved :: String -> MParser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

parens :: MParser a -> MParser a
parens = between (symbol "(") (symbol ")")

braces :: MParser a -> MParser a
braces = between (symbol "{") (symbol "}")

angles :: MParser a -> MParser a
angles = between (symbol "[") (symbol "]")

commaSep :: MParser a -> MParser [a]
commaSep p = p `sepBy` symbol ","

-- Types
ptype :: MParser Type
ptype = (TInt <$ reserved "Int")
    <|> (TBool <$ reserved "Bool")
    <|> (TVar <$> identifier)

-- Expressions
pexpr :: MParser Expr
pexpr = choice
  [ pLet
  , try pIf
  , do es <- some pterm
       return (foldl1 App es)
  ]

pterm :: MParser Expr
pterm = choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , try (do
        f <- identifier
        ty <- angles ptype
        args <- parens (commaSep pexpr)
        return (foldl App (TypeApp (Var f) ty) args))
  , Var <$> identifier
  , parens pexpr
  ]

-- Let/If
pLet :: MParser Expr
pLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- pexpr
  optional (symbol ";")
  e2 <- pexpr
  return (Let x e1 e2)

pIf :: MParser Expr
pIf = do
  reserved "if"
  c <- pexpr
  reserved "then"
  t <- pexpr
  reserved "else"
  f <- pexpr
  optional (symbol ";")
  return (If c t f)

-- Top-level function
pDef :: MParser TopLevel
pDef = do
  reserved "def"
  name <- identifier
  tyvars <- option [] (angles (commaSep identifier))
  args <- parens (commaSep ((,) <$> identifier <*> (symbol ":" *> ptype)))
  body <- braces (pexpr <* optional (symbol ";"))
  return (Def name tyvars args body)

-- Program
pProgram :: MParser Program
pProgram = many (sc *> pDef <* sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram "source"
-}

-- src/Parsing.hs
module Parsing (parseProgram) where

import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

import Data.Set (Set)
import qualified Data.Set as Set

reservedWords :: Set String
reservedWords = Set.fromList ["if", "then", "else", "true", "false", "let", "def", "Int", "Bool"]

type MParser = Parsec Void String

-- Lexer
sc :: MParser ()
sc = L.space space1 empty empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

identifier :: MParser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `Set.member` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

reserved :: String -> MParser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

parens :: MParser a -> MParser a
parens = between (symbol "(") (symbol ")")

braces :: MParser a -> MParser a
braces = between (symbol "{") (symbol "}")

angles :: MParser a -> MParser a
angles = between (symbol "[") (symbol "]")

commaSep :: MParser a -> MParser [a]
commaSep p = p `sepBy` symbol ","

-- Types
ptype :: MParser Type
ptype = (TInt <$ reserved "Int")
    <|> (TBool <$ reserved "Bool")
    <|> (TVar <$> identifier)

-- Expressions
pexpr :: MParser Expr
pexpr = choice
  [ pLet
  , try pIf
  , do es <- some pterm
       return (foldl1 App es)
  ]

pterm :: MParser Expr
pterm = choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ reserved "true"
  , BoolLit False <$ reserved "false"
  , try (do
        f <- identifier
        ty <- angles ptype
        args <- parens (commaSep pexpr)
        return (foldl App (TypeApp (Var f) ty) args))
  , Var <$> identifier
  , parens pexpr
  ]

-- Let/If
pLet :: MParser Expr
pLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- pexpr
  optional (symbol ";")
  e2 <- pexpr
  return (Let x e1 e2)

pIf :: MParser Expr
pIf = do
  reserved "if"
  c <- pexpr
  reserved "then"
  t <- pexpr <* optional (symbol ";")
  reserved "else"
  f <- pexpr <* optional (symbol ";")
  return (If c t f)

-- Top-level function
pDef :: MParser TopLevel
pDef = do
  reserved "def"
  name <- identifier
  tyvars <- option [] (angles (commaSep identifier))
  args <- parens (commaSep ((,) <$> identifier <*> (symbol ":" *> ptype)))
  body <- braces (pexpr <* optional (symbol ";"))
  return (Def name tyvars args body)

-- Program
pProgram :: MParser Program
pProgram = many (sc *> pDef <* sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram "source"
