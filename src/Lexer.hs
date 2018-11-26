module Lexer where

import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as Lazy
import qualified Text.Parsec.Token as Token

import Data.Functor.Identity
import Debug.Trace


reservedNames :: [String]
reservedNames = 
    [ "let"
    , "in"
    , "if"
    , "then"
    , "else"
    , "attribute"
    , "uniform"
    , "constant"
    , "varying"
    ]


reservedOps :: [String]
reservedOps = 
    [ "/"
    , "+"
    , "*"
    , "-"
    , "="
    , "\\"
    , "."
    ]

lexer :: Token.GenTokenParser Lazy.Text () Identity
lexer = Token.makeTokenParser $ Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = oneOf $ map head $ reservedOps 
  , Token.opLetter        = oneOf $ reservedOps >>= id
  , Token.reservedNames   = reservedNames
  , Token.reservedOpNames = reservedOps
  , Token.caseSensitive   = True
  }

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Token.semiSep1 lexer

semi :: Parser String
semi = Token.semi lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace lexer
  r <- p
  eof
  pure r

someFunc :: IO ()
someFunc = putStrLn "someFunc"
