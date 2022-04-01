{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Prelude hiding (foldr)

import Type
import Syntax

import Control.Monad.State
import Control.Monad.Except

import Data.Monoid
import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Semigroup, Monoid, Show)


data Unique = Unique { count :: Int }

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String Offset Offset
  deriving (Show, Eq, Ord)


glslStdLib :: TypeEnv
glslStdLib = TypeEnv $ Map.fromList 
    [ ("vec2", Forall [] (TCon Float `TArr` TCon Float `TArr` TCon Vec2)) 
    , ("vec3", Forall [] (TCon Float `TArr` TCon Float `TArr` TCon Float `TArr` TCon Vec3))
    , ("vec4", Forall [] (TCon Float `TArr` TCon Float `TArr` TCon Float `TArr` TCon Float `TArr` TCon Vec4))
    , ("mat2", Forall [] (TCon Float `TArr` TCon Float `TArr` TCon Float `TArr` TCon Float `TArr` TCon Mat2))
    , ("dot", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a") `TArr` TCon Float))
    , ("smoothstep", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a") `TArr` TVar (TV "a") `TArr` TVar (TV "a")))
    , ("step", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a") `TArr` TVar (TV "a")))
    , ("fract", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a")))
    , ("sin", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a")))
    , ("cos", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a")))
    , ("floor", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a")))
    , ("mix", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a") `TArr` TCon Float `TArr` TVar (TV "a")))
    , ("abs", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a")))
    , ("mod", Forall [TV "a"] (TVar (TV "a") `TArr` TCon Float `TArr` TVar (TV "a")))
    , ("clamp", Forall [TV "a"] (TVar (TV "a") `TArr` TCon Float `TArr` TCon Float `TArr` TVar (TV "a")))
    , ("atan", Forall [] (TCon Float `TArr` TCon Float `TArr` TCon Float))
    , ("length", Forall [TV "a"] (TVar (TV "a") `TArr` TCon Float))
    , ("min", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a") `TArr` TVar (TV "a")))
    , ("max", Forall [TV "a"] (TVar (TV "a") `TArr` TVar (TV "a") `TArr` TVar (TV "a")))
    , ("texture1D", Forall [] (TCon Sampler1D `TArr` TCon Float `TArr` TCon Vec4))
    , ("texture2D", Forall [] (TCon Sampler2D `TArr` TCon Vec2 `TArr` TCon Vec4))
    , ("texture3D", Forall [] (TCon Sampler3D `TArr` TCon Vec3 `TArr` TCon Vec4))
    ]


runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

-- TODO: this should return an error if the name already exists and isn't the same type
extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insertWith (flip const) x s env 

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ Map.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Type -> Binop -> Type
ops tv Add = tv `TArr` tv `TArr` tv
ops tv Mul = tv `TArr` tv `TArr` tv
ops tv Sub = tv `TArr` tv `TArr` tv
ops tv Div = tv `TArr` tv `TArr` tv
ops tv Eql = tv `TArr` tv `TArr` typeBool

lookupEnv :: TypeEnv -> (Var, Offset, Offset) -> Infer (Subst, Type)
lookupEnv (TypeEnv env) (x, start, end) =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x) start end
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

extendDecl :: TypeEnv -> Decl -> Infer (Subst, TypeEnv)
extendDecl env (name, e) = do
  (s, t) <- infer env e
  let env' = apply s env
      t'   = generalize env' t
  pure $ (s, env' `extend` (name, t'))

extendDecls :: TypeEnv -> [Decl] -> Infer (Subst, TypeEnv)
extendDecls env =
  foldM step (nullSubst, env)
  where
    step (s, e) decl = do
      (s1, e2) <- extendDecl e decl
      pure (s1 `compose` s, e2)

swizzleType :: String -> Type
swizzleType sw = case length sw of
    1 -> TCon Float
    2 -> TCon Vec2
    3 -> TCon Vec3
    4 -> TCon Vec4

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Var x start end -> lookupEnv env (x, start, end)

  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArr` t1)

  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArr t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Let decls e2 -> do
    (s1, env') <- extendDecls env decls
    (s2, t2) <- infer env' e2
    return (s2 `compose` s1, t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArr` tv `TArr` tv `TArr` tv)

  Op op e1 e2 -> do
    tv <- fresh
    inferPrim env [e1, e2] (ops tv op)

  Swizzle var sw -> do
    -- TODO Actually check that the swizzle is valid for the type and the name exists
    return (nullSubst, swizzleType sw)
    

  Lit (LInt _)  -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)
  Lit (LFloat _) -> return (nullSubst, typeFloat)

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . (TArr t))

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [Decl] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ParameterDecl (Uniform ty)):xs) = let
  newEnv = extend env $ (name, Forall [] $ TCon ty)
  in inferTop newEnv xs
inferTop env ((name, TypeAscription scheme):xs) = let
  newEnv = extend env $ (name, scheme)
  in inferTop newEnv xs
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
