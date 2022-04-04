module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

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

makeSpaceConsumer :: Parser () -> Parser ()
makeSpaceConsumer sp = L.space sp lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = makeSpaceConsumer $ skipSome $ oneOf " \t"

eolSpaceConsumer :: Parser ()
eolSpaceConsumer = makeSpaceConsumer space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- TODO: figure out negative parsing
integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '-')
    check x = if x `elem` reservedNames
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

swizzleStr :: Parser String
swizzleStr = lexeme $ some $ oneOf ['x', 'y', 'z', 'w', 'r', 'g', 'b', 'a']

discardNewline :: Parser ()
discardNewline = option () (void eol)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
