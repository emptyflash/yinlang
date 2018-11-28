module Type where

import qualified Syntax as S

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon S.GlslTypes
  | TArr Type Type
  deriving (Show, Eq, Ord)

infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt  = TCon S.Int

typeFloat :: Type
typeFloat  = TCon S.Float

typeBool :: Type
typeBool = TCon S.Bool
