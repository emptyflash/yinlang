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
  deriving (Show, Eq, Ord)

infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt  = TCon Int

typeFloat :: Type
typeFloat  = TCon Float

typeBool :: Type
typeBool = TCon Bool
