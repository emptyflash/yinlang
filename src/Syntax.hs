module Syntax where

import qualified Type as T

type Var = String
type Decl = (Var, Expr)

data Expr
  = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let [Decl] Expr
  | Lit Lit
  | If Expr Expr Expr
  | Fix Expr
  | Op Binop Expr Expr
  | Swizzle Var Var
  | ParameterDecl GlslParameter
  | TypeAscription T.Scheme
  deriving (Show, Eq, Ord)

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
