module Gen where

import Debug.Trace
import Infer
import Syntax
import Type


generateGlslType :: GlslTypes -> String
generateGlslType ty = case ty of
    Bool -> "bool"
    Int -> "int"
    Float -> "float"
    Vec2 -> "vec2"
    Vec3 -> "vec3"
    Vec4 -> "vec4"
    Mat2 -> "mat2"
    Mat3 -> "mat3"
    Mat4 -> "mat4"
    Sampler1D -> "sampler1D"
    Sampler2D -> "sampler2D"
    Sampler3D -> "sampler3D"

generateOp :: Binop -> String
generateOp op = case op of
    Add -> " + "
    Sub -> " - "
    Mul -> " * "
    Eql -> " == "
    Div -> " / "

generateLet :: TypeEnv -> [Decl] -> Expr -> String -> String
generateLet env [] inExpr state = state ++ "return " ++ (generateExpr env inExpr) ++ ";\n"
generateLet env ((var, expr):xs) inExpr state = let
  typeResult = inferExpr env expr
  (newEnv, newState) = case typeResult of
    Left error -> (env, state ++ (show error))
    Right scheme@(Forall [] (TCon ty)) -> (extend env (var, scheme), state ++ (generateGlslType ty) ++ " " ++ var ++ " = " ++ (generateExpr env expr) ++ ";\n")
  in generateLet newEnv xs inExpr newState

generateApp :: TypeEnv -> Expr -> Expr -> String
generateApp env (Var fn) expr = fn ++ "(" ++ generateExpr env expr
generateApp env (App a1 a2) expr = generateApp env a1 a2 ++ ", " ++ generateExpr env expr

generateExpr :: TypeEnv -> Expr -> String
generateExpr env expr = case expr of
  Var x -> x

  Let decls expr -> generateLet env decls expr ""

  Lit lit -> case lit of
    LInt int -> show int

    LBool bool -> show bool

    LFloat float -> show float

  App e1 e2 -> generateApp env e1 e2 ++ ")"

  If e1 e2 e3 -> generateExpr env e1 ++ " ? " ++ generateExpr env e2 ++ " : " ++ generateExpr env e3

  Op op e1 e2 -> "(" ++ generateExpr env e1 ++ generateOp op ++ generateExpr env e2 ++ ")"

  Swizzle v1 v2 -> v1 ++ "." ++ v2

getLastType :: Type -> GlslTypes
getLastType (TArr _ x) = getLastType x
getLastType (TCon x) = x

glslType :: Type -> GlslTypes
glslType (TCon x) = x

generateLam :: TypeEnv -> Expr -> Type -> String
generateLam env (Lam var expr) (TArr ty (TCon _)) = let
    glslTy = glslType ty
    newEnv = extend env (var, (Forall [] (TCon glslTy)))
    signature = generateGlslType glslTy ++ " " ++ var ++ ") {\n"
    body = case expr of
        expr@(Let _ _) -> generateExpr newEnv expr
        expr -> "return " ++ generateExpr newEnv expr ++ ";\n"
    in signature ++ body ++ "}\n\n"
generateLam env (Lam var expr@(Lam _ _)) (TArr ty1 ty2) = let
    glslTy =  glslType ty1
    newEnv = extend env (var, (Forall [] (TCon glslTy)))
    in generateGlslType glslTy ++ " " ++ var ++ ", " ++ generateLam newEnv expr ty2

generateDecl :: TypeEnv -> Decl -> String
generateDecl env (_, TypeAscription _) = ""
generateDecl env (var, ParameterDecl (Uniform ty)) = "uniform " ++ generateGlslType ty ++ " " ++ var ++ ";\n"
generateDecl env (var, lam@(Lam _ _)) = case typeof env var of
    Just (Forall _ ty) -> generateGlslType (getLastType ty) ++ " " ++ var ++ "(" ++ generateLam env lam ty
    a -> show a
generateDecl env (var, expr) = var ++ " = " ++ generateExpr env expr
