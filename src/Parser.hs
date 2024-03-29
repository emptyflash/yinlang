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
import qualified Type as T

import Syntax

type Parser = Parsec Void String
type StringError = ParseErrorBundle String Void

variable :: Parser Expr
variable = do
  start <- getOffset
  x <- L.identifier
  end <- getOffset
  return (Var x start end)

int :: Parser Expr
int = do
  n <- L.integer
  return (Lit (LInt n))

float :: Parser Expr
float = do
  f <- L.float
  pure . Lit $ LFloat f

bool :: Parser Expr
bool = (L.reserved "true" >> return (Lit (LBool True)))
   <|> (L.reserved "false" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  start <- getOffset
  L.reserved "\\"
  args <- many L.identifier
  L.reserved "->"
  body <- expr
  end <- getOffset
  return $ foldr (\v x -> Lam v x start end) body args

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
  start <- getOffset
  L.reserved "if"
  cond <- expr
  L.reserved "then"
  tr <- expr
  L.reserved "else"
  fl <- expr
  end <- getOffset
  return (If cond tr fl start end)

swizzle :: Parser Expr
swizzle = do
  name <- L.identifier
  L.symbol "."
  op <- L.swizzleStr
  pure $ Swizzle name op

aexp :: Parser Expr
aexp =
      L.parens expr
  <|> try float
  <|> bool
  <|> int
  <|> ifthen
  <|> letin
  <|> lambda
  <|> try swizzle
  <|> variable

term :: Parser Expr
term = do
    start <- getOffset
    x <- aexp
    let maybeApp = do xs <- many aexp 
                      end <- getOffset
                      return $ foldl (\x1 x2 -> App x1 x2 start end) x xs
    try maybeApp <|> return x

table :: [[Operator Parser Expr]]
table = 
    [ [ InfixL $ operatorOffset "*" Mul
      , InfixL $ operatorOffset "/" Div
      ]
    , [ InfixL $ operatorOffset "+" Add
      , InfixL $ operatorOffset "-" Sub
      ]
    , [ InfixL $ operatorOffset "==" Eql
      , InfixL $ operatorOffset ">" Gt
      , InfixL $ operatorOffset ">=" Gte
      , InfixL $ operatorOffset "<" Lt
      , InfixL $ operatorOffset "<=" Lte
      ]
    ]
    where 
    operatorOffset symbol binop = do
        start <- getOffset
        L.symbol symbol
        end <- getOffset
        return $ \e1 e2 -> Op binop e1 e2 start end

expr :: Parser Expr
expr = makeExprParser term table

fundecl :: Parser Decl
fundecl = do
  start <- getOffset
  name <- L.identifier
  args <- many L.identifier
  L.reserved "="
  body <-  expr
  end <- getOffset
  return $ (name, foldr (\v x -> Lam v x start end) body args)

tyLit :: Parser T.GlslTypes
tyLit = 
  L.symbol "Vec2" *> pure T.Vec2
  <|> L.symbol "Vec3" *> pure T.Vec3
  <|> L.symbol "Vec4" *> pure T.Vec4
  <|> L.symbol "Mat2" *> pure T.Mat2
  <|> L.symbol "Mat3" *> pure T.Mat3
  <|> L.symbol "Mat4" *> pure T.Mat4
  <|> L.symbol "Float" *> pure T.Float
  <|> L.symbol "Bool" *> pure T.Bool
  <|> L.symbol "Int" *> pure T.Int
  <|> L.symbol "Sampler1D" *> pure T.Sampler1D
  <|> L.symbol "Sampler2D" *> pure T.Sampler2D
  <|> L.symbol "Sampler3D" *> pure T.Sampler3D

scheme :: Parser T.Scheme
scheme = do
  types <- sepBy1 tyLit $ L.reserved "->"
  pure $ T.Forall [] $ foldr T.TArr (T.TCon $ last types) (map T.TCon $ init types)

typeAscription :: Parser Decl
typeAscription = do
  name <- L.identifier
  L.reserved ":"
  scheme <- scheme
  pure (name, TypeAscription scheme)

uniformdecl :: Parser Decl
uniformdecl = do
  L.reserved "uniform" 
  name <- L.identifier
  L.reserved ":"
  type_ <- tyLit
  pure (name, ParameterDecl $ Uniform type_)

decl :: Parser Decl
decl = 
  try fundecl 
  <|> typeAscription
  -- <|> constdecl 
  -- <|> attributedecl 
  <|> uniformdecl
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

parseModule ::  FilePath -> String -> Either StringError [Decl]
parseModule fname input = parse modl fname input
