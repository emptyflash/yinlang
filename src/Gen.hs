module Gen where

import Text.Megaparsec

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.List (intercalate, (\\))
import Data.Char (toLower)

import Data.Bifunctor
import Debug.Trace
import Infer
import Syntax
import Type
import qualified Parser as Parser

import Control.Monad.State as S
import Control.Monad.Writer
import Control.Monad.Except

-- Monad for code generation with error handling and state
type GenM = ExceptT String (S.State FunctionMapping)

type GenState = S.State FunctionMapping

-- Data structure for tracking generated anonymous functions
data FunctionMapping = FunctionMapping
  { functionCounter :: Int
  , functionMap :: Map.Map Expr (String, Type)
  }

-- Initial state for function mapping
initialFunctionMapping :: FunctionMapping
initialFunctionMapping = FunctionMapping 0 Map.empty

-- Run the generation monad
runGenM :: GenM a -> (Either String a, FunctionMapping)
runGenM gen = S.runState (runExceptT gen) initialFunctionMapping

-- Generate a unique function name
generateFunctionName :: GenM String
generateFunctionName = do
  mapping <- lift S.get
  let counter = functionCounter mapping
  lift $ S.put $ mapping { functionCounter = counter + 1 }
  return $ "anon_" ++ show counter

-- Register an anonymous function and return its generated name
registerAnonymousFunction :: Expr -> Type -> GenM String
registerAnonymousFunction expr ty = do
  name <- generateFunctionName
  mapping <- lift S.get
  lift $ S.put $ mapping { functionMap = Map.insert expr (name, ty) (functionMap mapping) }
  return name

-- Helper functions for the monad
throwGenError :: String -> GenM a
throwGenError = throwError

liftState :: S.State FunctionMapping a -> GenM a
liftState = lift

-- First pass: collect all anonymous functions and assign names
collectAnonymousFunctions :: TypeEnv -> [Decl] -> GenState ()
collectAnonymousFunctions env decls = do
  mapM_ (collectFromDecl env) decls

collectFromDecl :: TypeEnv -> Decl -> GenState ()
collectFromDecl env (_, expr) = collectFromExpr env expr

collectFromExpr :: TypeEnv -> Expr -> GenState ()
collectFromExpr env expr = case expr of
  App e1 e2 _ _ -> do
    collectFromExpr env e1
    collectFromExpr env e2

  Let decls body -> do
    -- Collect from declarations with proper environment extension
    let collectWithEnv env' (var, expr) = do
          collectFromExpr env' expr
          case inferExpr env' expr of
            Right scheme -> return (extend env' (var, scheme))
            Left _ -> return env'  -- Skip if type inference fails

    -- Process declarations sequentially, extending environment
    newEnv <- foldM collectWithEnv env decls
    collectFromExpr newEnv body

  If cond thenExpr elseExpr _ _ -> do
    collectFromExpr env cond
    collectFromExpr env thenExpr
    collectFromExpr env elseExpr

  Op _ e1 e2 _ _ -> do
    collectFromExpr env e1
    collectFromExpr env e2

  Lam var body _ _ -> do
    -- For anonymous functions in let expressions, we need to collect them
    -- Try to infer the type of the lambda
    case inferExpr env expr of
      Right (Forall _ ty) -> do
        -- Register the anonymous function with the inferred type
        mapping <- S.get
        let counter = functionCounter mapping
        let name = "anon_" ++ show counter
        let newMapping = mapping { functionCounter = counter + 1, functionMap = Map.insert expr (name, ty) (functionMap mapping) }
        S.put newMapping
        -- Continue collecting from the body
        collectFromExpr (extend env (var, Forall [] (getFirstType ty))) body
      Left _ -> return () -- Type error, skip

  _ -> return ()

-- Helper to get the first type in a function type
getFirstType :: Type -> Type
getFirstType (TArr t1 _) = t1
getFirstType t = t

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

-- Generate let expression with function mapping support (monadic version)
generateLetM :: TypeEnv -> [Decl] -> Expr -> GenM String
generateLetM env [] inExpr = do
  inExprCode <- generateExprM env inExpr
  return $ "return " ++ inExprCode ++ ";\n"
generateLetM env ((var, expr):xs) inExpr = do
  typeResult <- case inferExpr env expr of
    Left err -> throwGenError $ "Type error in let expression for variable '" ++ var ++ "': " ++ show err
    Right scheme -> return scheme

  let newEnv = extend env (var, typeResult)

  case typeResult of
    Forall [] (TCon ty) -> do
      exprCode <- generateExprM env expr
      restCode <- generateLetM newEnv xs inExpr
      return $ (generateGlslType ty) ++ " " ++ var ++ " = " ++ exprCode ++ ";\n" ++ restCode
    _ -> do
      restCode <- generateLetM newEnv xs inExpr
      return restCode

-- Original non-monadic version for backward compatibility
generateLetWithMapping :: TypeEnv -> FunctionMapping -> [Decl] -> Expr -> String -> String
generateLetWithMapping env mapping [] inExpr state = state ++ "return " ++ (generateExprWithMapping env mapping inExpr) ++ ";\n"
generateLetWithMapping env mapping ((var, expr):xs) inExpr state = let
  typeResult = inferExpr env expr
  (newEnv, newState) = case typeResult of
    Left err -> error $ "Type error in let expression for variable '" ++ var ++ "': " ++ show err
    Right scheme@(Forall [] (TCon ty)) -> (extend env (var, scheme), state ++ (generateGlslType ty) ++ " " ++ var ++ " = " ++ (generateExprWithMapping env mapping expr) ++ ";\n")
    Right scheme -> (extend env (var, scheme), state)
  in generateLetWithMapping newEnv mapping xs inExpr newState

-- Generate application with function mapping support (monadic version)
generateAppM :: TypeEnv -> Expr -> Expr -> GenM String
generateAppM env (Var fn _ _) expr = do
  exprCode <- generateExprM env expr
  return $ fn ++ "(" ++ exprCode
generateAppM env (App a1 a2 _ _) expr = do
  appCode <- generateAppM env a1 a2
  exprCode <- generateExprM env expr
  return $ appCode ++ ", " ++ exprCode
generateAppM env e1 e2 = do
  e1Code <- generateExprM env e1
  e2Code <- generateExprM env e2
  return $ e1Code ++ "(" ++ e2Code

-- Original non-monadic version for backward compatibility
generateAppWithMapping :: TypeEnv -> FunctionMapping -> Expr -> Expr -> String
generateAppWithMapping env mapping (Var fn _ _) expr = fn ++ "(" ++ generateExprWithMapping env mapping expr
generateAppWithMapping env mapping (App a1 a2 _ _) expr = generateAppWithMapping env mapping a1 a2 ++ ", " ++ generateExprWithMapping env mapping expr
generateAppWithMapping env mapping e1 e2 = generateExprWithMapping env mapping e1 ++ "(" ++ generateExprWithMapping env mapping e2

-- Original generateLet for backward compatibility
generateLet :: TypeEnv -> [Decl] -> Expr -> String -> String
generateLet env decls expr state = generateLetWithMapping env initialFunctionMapping decls expr state

-- Original generateApp for backward compatibility
generateApp :: TypeEnv -> Expr -> Expr -> String
generateApp env e1 e2 = generateAppWithMapping env initialFunctionMapping e1 e2

-- Generate expression with function mapping support (monadic version)
generateExprM :: TypeEnv -> Expr -> GenM String
generateExprM env expr = case expr of
  Var x _ _ -> return x

  Let decls expr -> generateLetM env decls expr

  Lit lit -> case lit of
    LInt int -> return $ show int
    LBool bool -> return $ map toLower $ show bool
    LFloat float -> return $ show float

  App e1 e2 _ _ -> do
    appCode <- generateAppM env e1 e2
    return $ appCode ++ ")"

  If e1 e2 e3 _ _ -> do
    cond <- generateExprM env e1
    thenExpr <- generateExprM env e2
    elseExpr <- generateExprM env e3
    return $ cond ++ " ? " ++ thenExpr ++ " : " ++ elseExpr

  Op op e1 e2 _ _ -> do
    left <- generateExprM env e1
    right <- generateExprM env e2
    return $ "(" ++ left ++ generateOp op ++ right ++ ")"

  Swizzle v1 v2 -> return $ v1 ++ "." ++ v2

  Lam _ _ _ _ -> do
    mapping <- lift get
    case Map.lookup expr (functionMap mapping) of
      Just (name, _) -> return name
      Nothing -> throwGenError "Unregistered anonymous function"

-- Original non-monadic version for backward compatibility
generateExprWithMapping :: TypeEnv -> FunctionMapping -> Expr -> String
generateExprWithMapping env mapping expr = case expr of
  Var x _ _ -> x

  Let decls expr -> generateLetWithMapping env mapping decls expr ""

  Lit lit -> case lit of
    LInt int -> show int

    LBool bool -> map toLower $ show bool

    LFloat float -> show float

  App e1 e2 _ _ -> generateAppWithMapping env mapping e1 e2 ++ ")"

  If e1 e2 e3 _ _ -> generateExprWithMapping env mapping e1 ++ " ? " ++ generateExprWithMapping env mapping e2 ++ " : " ++ generateExprWithMapping env mapping e3

  Op op e1 e2 _ _ -> "(" ++ generateExprWithMapping env mapping e1 ++ generateOp op ++ generateExprWithMapping env mapping e2 ++ ")"

  Swizzle v1 v2 -> v1 ++ "." ++ v2

  Lam _ _ _ _ -> case Map.lookup expr (functionMap mapping) of
    Just (name, _) -> name
    Nothing -> error "Unregistered anonymous function"

-- Original generateExpr for backward compatibility
generateExpr :: TypeEnv -> Expr -> String
generateExpr env expr = generateExprWithMapping env initialFunctionMapping expr

getLastType :: Type -> GlslTypes
getLastType (TArr _ x) = getLastType x
getLastType (TCon x) = x
getLastType t = Float  -- Default to Float for other types

glslType :: Type -> GlslTypes
glslType (TCon x) = x
glslType (TArr t1 t2) = glslType t2
glslType (TVar _) = Float  -- Default type variables to Float
glslType t = Float  -- Default fallback instead of error

-- Generate lambda function definition with function mapping support (monadic version)
generateLamM :: TypeEnv -> Expr -> Type -> GenM String
generateLamM env (Lam var expr _ _) (TArr ty (TCon _)) = do
  let glslTy = glslType ty
  let newEnv = extend env (var, (Forall [] (TCon glslTy)))
  body <- case expr of
    expr@(Let _ _) -> generateExprM newEnv expr
    expr -> do
      exprCode <- generateExprM newEnv expr
      return $ "return " ++ exprCode ++ ";\n"
  return $ generateGlslType glslTy ++ " " ++ var ++ ") {\n" ++ body ++ "}\n\n"
generateLamM env (Lam var expr@(Lam _ _ _ _) _ _) (TArr ty1 ty2) = do
  let glslTy = glslType ty1
  let newEnv = extend env (var, (Forall [] (TCon glslTy)))
  restCode <- generateLamM newEnv expr ty2
  return $ generateGlslType glslTy ++ " " ++ var ++ ", " ++ restCode

-- Original non-monadic version for backward compatibility
generateLamWithMapping :: TypeEnv -> FunctionMapping -> Expr -> Type -> String
generateLamWithMapping env mapping (Lam var expr _ _) (TArr ty (TCon _)) = let
    glslTy = glslType ty
    newEnv = extend env (var, (Forall [] (TCon glslTy)))
    signature = generateGlslType glslTy ++ " " ++ var ++ ") {\n"
    body = case expr of
        expr@(Let _ _) -> generateExprWithMapping newEnv mapping expr
        expr -> "return " ++ generateExprWithMapping newEnv mapping expr ++ ";\n"
    in signature ++ body ++ "}\n\n"
generateLamWithMapping env mapping (Lam var expr@(Lam _ _ _ _) _ _) (TArr ty1 ty2) = let
    glslTy =  glslType ty1
    newEnv = extend env (var, (Forall [] (TCon glslTy)))
    in generateGlslType glslTy ++ " " ++ var ++ ", " ++ generateLamWithMapping newEnv mapping expr ty2

-- Original generateLam for backward compatibility
generateLam :: TypeEnv -> Expr -> Type -> String
generateLam env expr ty = generateLamWithMapping env initialFunctionMapping expr ty

-- Helper function to collect all parameters from a nested lambda structure
collectLambdaParams :: Expr -> ([String], Expr)
collectLambdaParams (Lam var body _ _) =
  let (params, finalBody) = collectLambdaParams body
  in (var : params, finalBody)
collectLambdaParams body = ([], body)

-- Generate all anonymous function definitions
generateAnonymousFunctions :: TypeEnv -> FunctionMapping -> String
generateAnonymousFunctions env mapping =
  Map.foldrWithKey generateFunctionDef "" (functionMap mapping)
  where
    generateFunctionDef expr (name, ty) acc =
      case expr of
        Lam var body _ _ ->
          let (params, finalBody) = collectLambdaParams expr
          in if length params > 1
             then
               -- Multi-parameter lambda: generate single function with multiple parameters
               let paramTypes = collectParamTypes ty (length params)
                   paramDecls = zipWith (\p t -> generateGlslType t ++ " " ++ p) params paramTypes
                   signature = generateGlslType (getLastType ty) ++ " " ++ name ++ "(" ++ intercalate ", " paramDecls ++ ") {\n"
                   newEnv = foldl (\e (p, t) -> extend e (p, Forall [] (TCon t))) env (zip params paramTypes)
                   bodyCode = case finalBody of
                     finalBody@(Let _ _) -> generateExprWithMapping newEnv mapping finalBody
                     finalBody -> "return " ++ generateExprWithMapping newEnv mapping finalBody ++ ";\n"
               in signature ++ bodyCode ++ "}\n\n" ++ acc
             else
               -- Single parameter: generate single function
               let glslTy = glslType (getFirstType ty)
                   newEnv = extend env (var, Forall [] (TCon glslTy))
                   signature = generateGlslType (getLastType ty) ++ " " ++ name ++ "(" ++ generateGlslType glslTy ++ " " ++ var ++ ") {\n"
                   bodyCode = case body of
                     body@(Let _ _) -> generateExprWithMapping newEnv mapping body
                     body -> "return " ++ generateExprWithMapping newEnv mapping body ++ ";\n"
               in signature ++ bodyCode ++ "}\n\n" ++ acc
        _ -> acc

    -- Helper to collect parameter types from a function type
    collectParamTypes :: Type -> Int -> [GlslTypes]
    collectParamTypes ty n = take n $ go ty
      where
        go (TArr t1 t2) = glslType t1 : go t2
        go _ = []

-- Generate declaration with function mapping support (monadic version)
generateDeclM :: TypeEnv -> Decl -> GenM String
generateDeclM env (_, TypeAscription _) = return ""
generateDeclM env (var, ParameterDecl (Uniform ty)) = return $ "uniform " ++ generateGlslType ty ++ " " ++ var ++ ";\n"
generateDeclM env (var, lam@(Lam _ _ _ _)) = case typeof env var of
   Just (Forall _ ty) -> do
     lamCode <- generateLamM env lam ty
     return $ generateGlslType (getLastType ty) ++ " " ++ var ++ "(" ++ lamCode
   Nothing -> throwGenError $ "Type error: cannot determine type for lambda function '" ++ var ++ "'"
generateDeclM env (var, expr) = do
  exprCode <- generateExprM env expr
  return $ var ++ " = " ++ exprCode

-- Original non-monadic version for backward compatibility
generateDecl :: TypeEnv -> Decl -> String
generateDecl env (_, TypeAscription _) = ""
generateDecl env (var, ParameterDecl (Uniform ty)) = "uniform " ++ generateGlslType ty ++ " " ++ var ++ ";\n"
generateDecl env (var, lam@(Lam _ _ _ _)) = case typeof env var of
   Just (Forall _ ty) -> generateGlslType (getLastType ty) ++ " " ++ var ++ "(" ++ generateLam env lam ty
   Nothing -> error $ "Type error: cannot determine type for lambda function '" ++ var ++ "'"

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

-- Multi-pass compilation with anonymous function support (monadic version)
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
        Nothing -> Left "Missing main function with type Vec2 -> Vec4"
    let newDecls = renameMain decls

    -- Multi-pass code generation using monadic error handling
    -- Pass 1: Collect anonymous functions
    let (_, functionMapping) = runState (collectAnonymousFunctions newEnv newDecls) initialFunctionMapping

    -- Pass 2: Generate code using monadic generation with the collected function mapping
    let (genResult, finalMapping) = S.runState (runExceptT $ do
          declCodes <- mapM (generateDeclM newEnv) newDecls
          return $ concat declCodes) functionMapping

    case genResult of
      Left err -> Left err
      Right code -> do
        let functionDefs = generateAnonymousFunctions newEnv finalMapping
        pure $ functionDefs ++ code ++ "\n\nvoid main() { gl_FragColor = userEntrypoint(gl_FragCoord.xy); }"

-- Generate declarations with function mapping support
generateDeclWithMapping :: TypeEnv -> FunctionMapping -> Decl -> String
generateDeclWithMapping env mapping (_, TypeAscription _) = ""
generateDeclWithMapping env mapping (var, ParameterDecl (Uniform ty)) = "uniform " ++ generateGlslType ty ++ " " ++ var ++ ";\n"
generateDeclWithMapping env mapping (var, lam@(Lam _ _ _ _)) = case typeof env var of
   Just (Forall _ ty) -> generateGlslType (getLastType ty) ++ " " ++ var ++ "(" ++ generateLamWithMapping env mapping lam ty
   Nothing -> error $ "Type error: cannot determine type for lambda function '" ++ var ++ "'"

generateDeclWithMapping env mapping (var, expr) = var ++ " = " ++ generateExprWithMapping env mapping expr
