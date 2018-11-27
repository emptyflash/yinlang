{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( parseExpr
    , parseModule
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

import Data.Void
import Control.Monad (void)
import Control.Monad.Combinators.Expr

import qualified Lexer as L

import Syntax

type Parser = Parsec Void String
type StringError = ParseErrorBundle String Void

variable :: Parser Expr
variable = do
  x <- L.identifier
  return (Var x)

int :: Parser Expr
int = do
  n <- L.integer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool = (L.reserved "true" >> return (Lit (LBool True)))
   <|> (L.reserved "false" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  L.reserved "\\"
  args <- many L.identifier
  L.reserved "->"
  body <- expr
  return $ foldr Lam body args

letdecl :: Parser Decl
letdecl = do
  x <- L.identifier
  L.reserved "="
  e <- expr
  L.eolSpaceConsumer
  pure (x, e)

letin :: Parser Expr
letin = do
  L.reserved "let"
  L.eolSpaceConsumer
  decls <- some letdecl
  L.reserved "in"
  L.eolSpaceConsumer
  e <- expr
  return (Let decls e)

ifthen :: Parser Expr
ifthen = do
  L.reserved "if"
  cond <- expr
  L.reserved "then"
  tr <- expr
  L.reserved "else"
  fl <- expr
  return (If cond tr fl)

aexp :: Parser Expr
aexp =
      L.parens expr
  <|> bool
  <|> int
  <|> ifthen
  <|> letin
  <|> lambda
  <|> variable

term :: Parser Expr
term = aexp >>= \x ->
                try (many aexp >>= \xs -> return (foldl App x xs))
                <|> return x

table :: [[Operator Parser Expr]]
table = 
    [ [ InfixL (Op Swizzle <$ L.symbol ".")
      ]
    , [ InfixL (Op Mul <$ L.symbol "*")
      , InfixL (Op Div <$ L.symbol "/")
      ]
    , [ InfixL (Op Add <$ L.symbol "+")
      , InfixL (Op Sub <$ L.symbol "-")
      ]
    , [ InfixL (Op Eql <$ L.symbol "==")
      ]
    ]

expr :: Parser Expr
expr = makeExprParser term table

fundecl :: Parser Decl
fundecl = do
  name <- L.identifier
  args <- many L.identifier
  L.reserved "="
  body <-  expr
  return $ (name, foldr Lam body args)

tyLit :: Parser Expr
tyLit = 
  L.symbol "vec2" *> pure (Ty Vec2)
  <|> L.symbol "vec3" *> pure (Ty Vec3)
  <|> L.symbol "vec4" *> pure (Ty Vec4)
  <|> L.symbol "mat2" *> pure (Ty Mat4)
  <|> L.symbol "mat2" *> pure (Ty Mat2)
  <|> L.symbol "mat4" *> pure (Ty Mat4)
  <|> L.symbol "float" *> pure (Ty Float)

constdecl :: Parser Decl
constdecl = do
  L.reserved "constant" 
  name <- L.identifier
  type_ <- tyLit
  pure (name, type_)

attributedecl :: Parser Decl
attributedecl = do
  L.reserved "attribute" 
  name <- L.identifier
  type_ <- tyLit
  pure (name, type_)

uniformdecl :: Parser Decl
uniformdecl = do
  L.reserved "uniform" 
  name <- L.identifier
  type_ <- tyLit
  pure (name, type_)

varyingdecl :: Parser Decl
varyingdecl = do
  L.reserved "varying" 
  name <- L.identifier
  type_ <- tyLit
  pure (name, type_)

decl :: Parser Decl
decl = 
  fundecl 
  -- <|> constdecl 
  -- <|> attributedecl 
  -- <|> uniformdecl
  -- <|> varyingdecl

top :: Parser Decl
top = do
  L.eolSpaceConsumer
  d <- decl
  pure d

modl ::  Parser [Decl]
modl = do
  d <- some $ try top
  L.eolSpaceConsumer
  eof
  pure d

parseExpr :: String -> Either StringError Expr
parseExpr input = parse expr "<stdin>" input

parseModule ::  FilePath -> String -> Either StringError [(String, Expr)]
parseModule fname input = parse modl fname input