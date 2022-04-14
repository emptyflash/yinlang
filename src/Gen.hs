module Gen where

import Text.Megaparsec

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Char (toLower)

import Data.Bifunctor
import Debug.Trace
import Infer
import Syntax
import Type
import qualified Parser as Parser


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
    Gt -> " > "
    Gte -> " >= "
    Lt -> " < "
    Lte -> " <= "

generateLet :: TypeEnv -> [Decl] -> Expr -> String -> String
generateLet env [] inExpr state = state ++ "return " ++ (generateExpr env inExpr) ++ ";\n"
generateLet env ((var, expr):xs) inExpr state = let
  typeResult = inferExpr env expr
  (newEnv, newState) = case typeResult of
    Left error -> undefined
    Right scheme@(Forall [] (TCon ty)) -> (extend env (var, scheme), state ++ (generateGlslType ty) ++ " " ++ var ++ " = " ++ (generateExpr env expr) ++ ";\n")
    Right scheme -> undefined
  in generateLet newEnv xs inExpr newState

generateApp :: TypeEnv -> Expr -> Expr -> String
generateApp env (Var fn _ _) expr = fn ++ "(" ++ generateExpr env expr
generateApp env (App a1 a2 _ _) expr = generateApp env a1 a2 ++ ", " ++ generateExpr env expr

generateExpr :: TypeEnv -> Expr -> String
generateExpr env expr = case expr of
  Var x _ _ -> x

  Let decls expr -> generateLet env decls expr ""

  Lit lit -> case lit of
    LInt int -> show int

    LBool bool -> map toLower $ show bool

    LFloat float -> show float

  App e1 e2 _ _ -> generateApp env e1 e2 ++ ")"

  If e1 e2 e3 _ _ -> generateExpr env e1 ++ " ? " ++ generateExpr env e2 ++ " : " ++ generateExpr env e3

  Op op e1 e2 _ _ -> "(" ++ generateExpr env e1 ++ generateOp op ++ generateExpr env e2 ++ ")"

  Swizzle v1 v2 -> v1 ++ "." ++ v2

getLastType :: Type -> GlslTypes
getLastType (TArr _ x) = getLastType x
getLastType (TCon x) = x

glslType :: Type -> GlslTypes
glslType (TCon x) = x

generateLam :: TypeEnv -> Expr -> Type -> String
generateLam env (Lam var expr _ _) (TArr ty (TCon _)) = let
    glslTy = glslType ty
    newEnv = extend env (var, (Forall [] (TCon glslTy)))
    signature = generateGlslType glslTy ++ " " ++ var ++ ") {\n"
    body = case expr of
        expr@(Let _ _) -> generateExpr newEnv expr
        expr -> "return " ++ generateExpr newEnv expr ++ ";\n"
    in signature ++ body ++ "}\n\n"
generateLam env (Lam var expr@(Lam _ _ _ _) _ _) (TArr ty1 ty2) = let
    glslTy =  glslType ty1
    newEnv = extend env (var, (Forall [] (TCon glslTy)))
    in generateGlslType glslTy ++ " " ++ var ++ ", " ++ generateLam newEnv expr ty2

generateDecl :: TypeEnv -> Decl -> String
generateDecl env (_, TypeAscription _) = ""
generateDecl env (var, ParameterDecl (Uniform ty)) = "uniform " ++ generateGlslType ty ++ " " ++ var ++ ";\n"
generateDecl env (var, lam@(Lam _ _ _ _)) = case typeof env var of
   Just (Forall _ ty) -> generateGlslType (getLastType ty) ++ " " ++ var ++ "(" ++ generateLam env lam ty
   Nothing -> undefined -- TODO this whole thing should be a state error t monad stack

generateDecl env (var, expr) = var ++ " = " ++ generateExpr env expr


instance ShowErrorComponent Infer.TypeError where
    showErrorComponent (Infer.UnboundVariable var _ _) = "Variable " ++ var ++ " is unbound"
    showErrorComponent (Infer.UnificationFail t1 t2 _ _) = "Type mismatch: expected " ++ show t1 ++ " but found " ++ show t2
    showErrorComponent err = show err

    errorComponentLen (Infer.UnboundVariable _ start end) = end - start
    errorComponentLen (Infer.UnificationFail _ _ start end) = end - start
    errorComponentLen _ = 0

renameMapKey :: Ord a => a -> a -> Map.Map a b -> Map.Map a b
renameMapKey old new m =
    case Map.lookup old m of
        Nothing -> m
        Just v -> Map.insert new v $ Map.delete old m

renameMainType :: Infer.TypeEnv -> Infer.TypeEnv
renameMainType (Infer.TypeEnv env) = Infer.TypeEnv $ renameMapKey "main" "userEntrypoint" env

renameMain :: [Decl] -> [Decl]
renameMain (("main", expr) : xs) = ("userEntrypoint", expr) : renameMain xs
renameMain (x : xs) = x : renameMain xs
renameMain [] = []

prettyShowErr prog err = let
    start = case err of
        Infer.UnboundVariable _ start _ -> start
        Infer.UnificationFail _ _ start _ -> start

    initialState = PosState
          { pstateInput = prog
          , pstateOffset = 0
          , pstateSourcePos = initialPos ""
          , pstateTabWidth = defaultTabWidth
          , pstateLinePrefix = ""
          }
    errorBundle = ParseErrorBundle
          { bundleErrors = NonEmpty.fromList [FancyError start $ Set.fromList [ErrorCustom err]]
                        -- ^ A collection of 'ParseError's that is sorted by parse error offsets
          , bundlePosState = initialState
                        -- ^ State that is used for line\/column calculation
          }
    in errorBundlePretty errorBundle

compileProgram :: String -> Either String String
compileProgram prog = do
    decls <- first errorBundlePretty $ Parser.parseModule "<stdin>" prog
    env <- case Infer.inferTop Infer.glslStdLib decls of
        Left err -> let 
            in Left $ prettyShowErr prog err
        res -> first show $ res
    newEnv <- case Infer.typeof env "main" of
        Just (Forall [] (TCon Vec2 `TArr` TCon Vec4)) -> Right $ renameMainType env
        Just scheme -> Left $ "Missing main function with correct type. Expected: Vec2 -> Vec4, Found: " ++ show scheme
    let newDecls = renameMain decls
    let code = newDecls >>= Gen.generateDecl newEnv 
    pure $ code ++ "\n\nvoid main() { gl_FragColor = userEntrypoint(gl_FragCoord.xy); }"
