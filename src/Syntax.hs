module Syntax where

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
  | Ty TyLit
  deriving (Show, Eq, Ord)

data TyLit
  = Float
  | Vec2
  | Vec3
  | Vec4
  | Mat2
  | Mat3
  | Mat4
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql | Div | Swizzle
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Expr deriving (Show, Eq)
