module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data GlslTypes
  = Bool
  | Int
  | Float
  | Vec2
  | Vec3
  | Vec4
  | Mat2
  | Mat3
  | Mat4
  | Sampler1D
  | Sampler2D
  | Sampler3D
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon GlslTypes
  | TArr Type Type
  deriving (Eq, Ord)

instance Show Type where
    show (TVar (TV var)) = "type variable " ++ var
    show (TCon ty) = show ty
    show (TArr t1 t2) = show t1 ++ " -> " ++ show t2

infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt  = TCon Int

typeFloat :: Type
typeFloat  = TCon Float

typeBool :: Type
typeBool = TCon Bool
