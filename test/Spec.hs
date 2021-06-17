import Data.Bifunctor
import System.Directory
import Test.Hspec
import qualified Data.Map as Map

import qualified Parser as P

import Syntax
import Lexer
import Infer
import Type
import Gen



main :: IO ()
main = hspec $ do
    describe "inference" $ do
        it "should infer integers" $ do
            let expr = Lit $ LInt 1
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeInt)

        it "should infer operators" $ do
            let expr = (Op Add (Lit $ LFloat 1) (Lit $ LFloat 1))
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeFloat)

        it "should infer let exprs" $ do
            let expr = (Let [ ("a", Lit $ LInt 1)
                            , ("b", Var "a")] (Var "b"))
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeInt)

        it "should infer complex exprs" $ do
            let expr = Let [ ("b", Lit $ LInt 2)
                           , ("c", Lit $ LInt 3)
                           , ("a", If (Lit $ LBool True) 
                                     (Op Sub (Var "b") (Var "b")) 
                                     (Op Add (Var "c") (Var "c")))
                           ]
                        (Op Mul (Var "a") (Var "c"))
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeInt)

        it "Handle uniform declartions" $ do
            let decls = [("u_audio", ParameterDecl $ Uniform Vec4), ("test", (Var "u_audio"))]
            let Right env = inferTop emptyTyenv decls
            putStrLn $ show env
            let res = typeof env "test"
            res `shouldBe` Just (Forall [] $ TCon Vec4)

        it "Handle type ascription" $ do
            let decls = [("test",TypeAscription (Forall [] (TArr (TCon Vec2) (TCon Vec2))))]
            let Right env = inferTop emptyTyenv decls
            putStrLn $ show env
            let res = typeof env "test"
            res `shouldBe` Just (Forall [] (TArr (TCon Vec2) (TCon Vec2)))

        -- TODO this should just be an error
        it "should ignore type of function if ascribed (for now)" $ do
            let decls = [ ("test", TypeAscription (Forall [] (TArr (TCon Vec2) (TCon Vec2))))
                        , ("test", Lam "n" (Var "n"))
                        ]
            let Right env = inferTop emptyTyenv decls
            putStrLn $ show env
            let res = typeof env "test"
            res `shouldBe` Just (Forall [] (TArr (TCon Vec2) (TCon Vec2)))

        it "should typecheck std lib" $ do
            path <- makeAbsolute "std.yin"
            stdLib <- readFile path
            let exprs = first show $ P.parseModule "std.yin" $ stdLib 
            let res = exprs >>= (first show . inferTop glslStdLib )
            case res of
                Right env -> do { putStrLn $ show env; True `shouldBe` True }
                Left err -> do { putStrLn $ show err; False `shouldBe` True }


    describe "parser" $ do
        it "should parse addition" $ do
            let expr = "1.0 + 1.0"
            let result = P.parseExpr expr
            result `shouldBe` Right (Op Add (Lit $ LFloat 1.0) (Lit $ LFloat 1.0))

        it "should parse multiplication" $ do
            let expr = "1 * 1"
            let result = P.parseExpr expr
            result `shouldBe` Right (Op Mul (Lit $ LInt 1) (Lit $ LInt 1))

        it "should parse swizzle" $ do
            let expr = "a.xyz"
            let result = P.parseExpr expr
            result `shouldBe` Right (Swizzle "a" "xyz")

        it "should parse nested application" $ do
            let expr = "a 1 1"
            let result = P.parseExpr expr
            result `shouldBe` Right (App (App (Var "a") (Lit $ LInt 1)) (Lit $ LInt 1))

        it "should parse simple let expr" $ do
            let expr = "let a = 1 in a"
            let result = P.parseExpr expr
            result `shouldBe` Right (Let [("a", Lit $ LInt 1)] (Var "a"))

        it "should parse multi let expr" $ do
            let expr = "let\na = 1\n\nb = 2\n\nin \na"
            let result = P.parseExpr expr
            result `shouldBe` Right (Let [("a", Lit $ LInt 1),
                                          ("b", Lit $ LInt 2)] (Var "a"))

        it "should parse complex exprs" $ do
            let expr = "let a = if true then b - b else c + c in a * c"
            let result = P.parseExpr expr
            result `shouldBe` Right (Let [("a", If (Lit $ LBool True) (Op Sub (Var "b") (Var "b")) (Op Add (Var "c") (Var "c")))] (Op Mul (Var "a") (Var "c")))

        it "should parse uniform declartions" $ do
            let expr = "uniform u_audio : Vec4"
            let result = P.parseModule "test" expr
            result `shouldBe` Right [("u_audio", ParameterDecl $ Uniform Vec4)]
            
        it "should parse type ascription" $ do
            let expr = "test : Float -> Vec2 -> Vec3 -> Vec4"
            let result = P.parseModule "test" expr
            result `shouldBe` Right [("test",TypeAscription (Forall [] (TArr (TCon Float) (TArr (TCon Vec2) (TArr (TCon Vec3) (TCon Vec4))))))]

        it "should parse the std lib" $ do
            path <- makeAbsolute "std.yin"
            stdLib <- readFile path
            case P.parseModule "std.yin" stdLib of
                Right a -> do { putStrLn $ show a; True `shouldBe` True }
                Left err -> do { putStrLn $ show err; False `shouldBe` True }

    describe "code generation" $ do
        
        it "should generate simple let expr" $ do
            let expr = (Let [ ("a", Lit $ LInt 1)
                            , ("b", Var "a")] (Var "b"))
            let result = generateExpr glslStdLib expr
            result `shouldBe` "int a = 1;\nint b = a;\nreturn b;\n"

        it "should generate single application" $ do
            let expr = App (Var "func") (Lit (LFloat 0.5))
            let result = generateExpr glslStdLib expr
            result `shouldBe` "func(0.5)"

        it "should generate triple application" $ do
            let expr = App (App (App (Var "vec3") (Lit (LFloat 0.5))) (Lit (LFloat 0.5))) (Lit (LFloat 0.5))
            let result = generateExpr glslStdLib expr
            result `shouldBe` "vec3(0.5, 0.5, 0.5)"

        it "should generate declarations" $ do
            let decl = ("circle",Lam "st" (Lam "r" (Lit $ LFloat 1.0)))
            let env = TypeEnv $ Map.fromList [ ("circle",Forall [] (TArr (TCon Vec2) (TArr (TCon Float) (TCon Float)))) ]
            let result = generateDecl env decl
            result `shouldBe` "float circle(vec2 st, float r) {\nreturn 1.0;\n}\n\n"

        it "should generate functions with arbitrary parameters" $ do
            let decl = ("func",Lam "a" (Lam "b" (Lam "c" (Lam "d" (Lit $ LFloat 1.0)))))
            let env = TypeEnv $ Map.fromList [ ("func",Forall [] (TArr (TCon Vec2) (TArr (TCon Vec2) (TArr (TCon Vec2) (TArr (TCon Vec2) (TCon Float)))))) ]
            let result = generateDecl env decl
            result `shouldBe` "float func(vec2 a, vec2 b, vec2 c, vec2 d) {\nreturn 1.0;\n}\n\n"

        it "should generate uniform declartions" $ do
            let decl = ("u_audio", ParameterDecl $ Uniform Vec4)
            let env = emptyTyenv
            let result = generateDecl env decl
            result `shouldBe` "uniform vec4 u_audio;\n"

        it "should ignore type ascription" $ do
            let decl = ("test",TypeAscription (Forall [] (TArr (TCon Vec2) (TCon Vec2))))
            let env = emptyTyenv
            let result = generateDecl env decl
            result `shouldBe` ""

        it "should generate the stdlib" $ do
            path <- makeAbsolute "std.yin"
            stdLib <- readFile path
            let exprs = first show $ P.parseModule "std.yin" $ stdLib 
            let Right env = exprs >>= (first show . inferTop glslStdLib )
            let Right decls = P.parseModule "std.yin" stdLib 
            let result = decls >>= generateDecl env
            putStrLn result
            True `shouldBe` True
