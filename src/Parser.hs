{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( parseExpr
    , parseModule
    ) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import qualified Data.Text.Lazy as Lazy
import Data.Functor.Identity

import Lexer
import Syntax


variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

int :: Parser Expr
int = do
  n <- Token.natural lexer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool = (reserved "true" >> return (Lit (LBool True)))
    <|> (reserved "false" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

letdecl :: Parser Decl
letdecl = do
  x <- identifier
  reservedOp "="
  e <- expr
  pure (x, e)

letin :: Parser Expr
letin = do
  reserved "let"
  decls <- sepBy1 letdecl (try $ symbol "\n")
  reserved "in"
  e <- expr
  return (Let decls e)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

aexp :: Parser Expr
aexp =
      parens expr
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

type Op a = Ex.Operator Lazy.Text () Identity a
type Operators a = Ex.OperatorTable Lazy.Text () Identity a

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
table = 
    [ [ infixOp "." (Op Swizzle) Ex.AssocLeft
      ]
    , [ infixOp "*" (Op Mul) Ex.AssocLeft
      , infixOp "/" (Op Div) Ex.AssocLeft
      ]
    , [ infixOp "+" (Op Add) Ex.AssocLeft
      , infixOp "-" (Op Sub) Ex.AssocLeft
      ]
    , [ infixOp "==" (Op Eql) Ex.AssocLeft
      ]
    ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

fundecl :: Parser Decl
fundecl = do
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return $ (name, foldr Lam body args)

tyLit :: Parser Expr
tyLit = 
  string "vec2" *> pure (Ty Vec2)
  <|> string "vec3" *> pure (Ty Vec3)
  <|> string "vec4" *> pure (Ty Vec4)
  <|> string "mat2" *> pure (Ty Mat4)
  <|> string "mat2" *> pure (Ty Mat2)
  <|> string "mat4" *> pure (Ty Mat4)
  <|> string "float" *> pure (Ty Float)

constdecl :: Parser Decl
constdecl = do
  reserved "constant" 
  name <- identifier
  type_ <- tyLit
  pure (name, type_)

attributedecl :: Parser Decl
attributedecl = do
  reserved "attribute" 
  name <- identifier
  type_ <- tyLit
  pure (name, type_)

uniformdecl :: Parser Decl
uniformdecl = do
  reserved "uniform" 
  name <- identifier
  type_ <- tyLit
  pure (name, type_)

varyingdecl :: Parser Decl
varyingdecl = do
  reserved "varying" 
  name <- identifier
  type_ <- tyLit
  pure (name, type_)

decl :: Parser Decl
decl = 
  fundecl 
  <|> constdecl 
  <|> attributedecl 
  <|> uniformdecl
  <|> varyingdecl

top :: Parser Decl
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Decl]
modl = many top

parseExpr :: Lazy.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

parseModule ::  FilePath -> Lazy.Text -> Either ParseError [(String, Expr)]
parseModule fname input = parse (contents modl) fname input
