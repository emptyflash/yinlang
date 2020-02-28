module Gen where

import Infer
import Syntax
import Type


generateType :: GlslTypes -> String
generateType ty = case ty of
    Bool -> "bool"
    Int -> "int"
    Float -> "float"
    Vec2 -> "vec2"
    Vec3 -> "vec3"


generateLet :: TypeEnv -> [Decl] -> Expr -> String -> String
generateLet env [] inExpr state = state ++ "return " ++ (generateExpr env inExpr) ++ ";\n"
generateLet env ((var, expr):xs) inExpr state = let
  typeResult = inferExpr env expr
  (newEnv, newState) = case typeResult of
    Left error -> (env, state ++ (show error))
    Right scheme@(Forall [] (TCon ty)) -> (extend env (var, scheme), state ++ (generateType ty) ++ " " ++ var ++ " = " ++ (generateExpr env expr) ++ ";\n")
  in generateLet newEnv xs inExpr newState

genApp :: TypeEnv -> Expr -> Expr -> String
genApp env (Var fn) expr = fn ++ "(" ++ generateExpr env expr
genApp env (App app1expr1 app1expr2) (App app2expr1 app2expr2) = genApp env app1expr1 app1expr1 ++ ", " ++ genApp env app2expr1 app2expr2
genApp env (App app1 app2) (Lit lit) = genApp env app1 app2 ++ ", " ++ generateExpr env (Lit lit) ++ ")"

generateExpr :: TypeEnv -> Expr -> String
generateExpr env expr = case expr of
  Var x -> x

  Let decls expr -> generateLet env decls expr ""

  Lit lit -> case lit of
    LInt int -> show int

    LBool bool -> show bool

    LFloat float -> show float

  App exp1 exp2 -> genApp env exp1 exp2
