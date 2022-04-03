module Syntax where

import qualified Type as T

type Var = String
type Decl = (Var, Expr)

type Offset = Int
type Offsets = (Offset, Offset)

data Expr
  = Var Var Offset Offset
  | App Expr Expr Offset Offset
  | Lam Var Expr Offset Offset
  | Let [Decl] Expr
  | Lit Lit
  | If Expr Expr Expr Offset Offset
  | Fix Expr
  | Op Binop Expr Expr Offset Offset
  | Swizzle Var Var
  | ParameterDecl GlslParameter
  | TypeAscription T.Scheme
  deriving (Show, Eq, Ord)

offsetsFromExpr (Var _ s e) = (s, e)
offsetsFromExpr (App _ _ s e) = (s, e)
offsetsFromExpr (Lam _ _ s e) = (s, e)
offsetsFromExpr (If _ _ _ s e) = (s, e)
offsetsFromExpr (Op _ _ _ s e) = (s, e)
offsetsFromExpr _ = (0, 0)

data GlslParameter
  = Uniform T.GlslTypes
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  | LFloat Float
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql | Div
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Expr deriving (Show, Eq)
