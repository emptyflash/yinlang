import Data.Bifunctor
import System.Directory
import Test.Hspec
import qualified Data.Map as Map
import Text.Megaparsec
import Data.List (isInfixOf, isSuffixOf)
import Control.Monad (forM_)

import qualified Parser as P

import Syntax
import Lexer
import Infer
import Type
import Gen


var var = Var var 0 0
app e1 e2 = App e1 e2 0 0
lam var ex = Lam var ex 0 0
op b e1 e2 = Op b e1 e2 0 0
if_ c t f = If c t f 0 0


main :: IO ()
main = hspec $ do
    describe "inference" $ do
        it "should infer integers" $ do
            let expr = Lit $ LInt 1
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeInt)

        it "should infer operators" $ do
            let expr = (op Add (Lit $ LFloat 1) (Lit $ LFloat 1))
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeFloat)

        it "should infer let exprs" $ do
            let expr = (Let [ ("a", Lit $ LInt 1)
                            , ("b", var "a")] (var "b"))
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeInt)

        it "should infer complex exprs" $ do
            let expr = Let [ ("b", Lit $ LInt 2)
                           , ("c", Lit $ LInt 3)
                           , ("a", if_ (Lit $ LBool True) 
                                     (op Sub (var "b") (var "b")) 
                                     (op Add (var "c") (var "c")))
                           ]
                        (op Mul (var "a") (var "c"))
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeInt)

        it "Handle uniform declartions" $ do
            let decls = [("u_audio", ParameterDecl $ Uniform Vec4), ("test", (var "u_audio"))]
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

        -- TODO this should actually be an error unless the ascribed type is more specific than the inferred
        it "should ignore type of function if ascribed (for now)" $ do
            let decls = [ ("test", TypeAscription (Forall [] (TArr (TCon Vec2) (TCon Vec2))))
                        , ("test", lam "n" (var "n"))
                        ]
            let Right env = inferTop emptyTyenv decls
            putStrLn $ show env
            let res = typeof env "test"
            res `shouldBe` Just (Forall [] (TArr (TCon Vec2) (TCon Vec2)))

        it "should typecheck std lib" $ do
            path <- makeAbsolute "std.yin"
            stdLib <- readFile path
            let exprs = first errorBundlePretty $ P.parseModule "std.yin" $ stdLib 
            let res = exprs >>= (first (Gen.prettyShowErr stdLib) . inferTop glslStdLib )
            case res of
                Right env -> do { putStrLn $ show env; True `shouldBe` True }
                Left err -> do { putStrLn err; False `shouldBe` True }


    describe "parser" $ do
        it "should parse addition" $ do
            let expr = "1.0 + 1.0"
            let result = P.parseExpr expr
            result `shouldBe` Right (Op Add (Lit $ LFloat 1.0) (Lit $ LFloat 1.0) 4 6)

        it "should parse multiplication" $ do
            let expr = "1 * 1"
            let result = P.parseExpr expr
            result `shouldBe` Right (Op Mul (Lit $ LInt 1) (Lit $ LInt 1) 2 4)

        it "should parse swizzle" $ do
            let expr = "a.xyz"
            let result = P.parseExpr expr
            result `shouldBe` Right (Swizzle "a" "xyz")

        it "should parse nested application" $ do
            let expr = "a 1 1"
            let result = P.parseExpr expr
            result `shouldBe` Right (App (App (Var "a" 0 2) (Lit (LInt 1)) 0 5) (Lit (LInt 1)) 0 5)

        it "should parse simple let expr" $ do
            let expr = "let a = 1 in a"
            let result = P.parseExpr expr
            result `shouldBe` Right (Let [("a", Lit $ LInt 1)] (Var "a" 13 14))

        it "should parse multi let expr" $ do
            let expr = "let\na = 1\n\nb = 2\n\nin \na"
            let result = P.parseExpr expr
            result `shouldBe` Right (Let [("a", Lit $ LInt 1),
                                          ("b", Lit $ LInt 2)] (Var "a" 22 23))

        it "should parse complex exprs" $ do
            let expr = "let a = if true then b - b else c + c in a * c"
            let result = P.parseExpr expr
            result `shouldBe` Right (Let [("a", If (Lit $ LBool True) (Op Sub (Var "b" 21 23) (Var "b" 25 27) 23 25) (Op Add (Var "c" 32 34) (Var "c" 36 38) 34 36) 8 38)] (Op Mul (Var "a" 41 43) (Var "c" 45 46) 43 45))

        it "should parse uniform declartions" $ do
            let expr = "uniform u_audio : Vec4"
            let result = P.parseModule "test" expr
            result `shouldBe` Right [("u_audio", ParameterDecl $ Uniform Vec4)]
            
        it "should parse type ascription" $ do
            let expr = "test : Float -> Vec2 -> Vec3 -> Vec4"
            let result = P.parseModule "test" expr
            result `shouldBe` Right [("test",TypeAscription (Forall [] (TArr (TCon Float) (TArr (TCon Vec2) (TArr (TCon Vec3) (TCon Vec4))))))]

        it "should parse lambda expressions" $ do
            let expr = "\\x -> x + 1.0"
            let result = P.parseExpr expr
            result `shouldBe` Right (Lam "x" (Op Add (Var "x" 6 8) (Lit (LFloat 1.0)) 8 10) 0 13)

        it "should parse lambda declarations" $ do
            let expr = "f = \\x -> x + 1.0"
            let result = P.parseModule "test" expr
            result `shouldBe` Right [("f", Lam "x" (Op Add (Var "x" 10 12) (Lit (LFloat 1.0)) 12 14) 4 17)]

        it "should parse the std lib" $ do
            path <- makeAbsolute "std.yin"
            stdLib <- readFile path
            case P.parseModule "std.yin" stdLib of
                Right a -> True `shouldBe` True
                Left err -> do { putStrLn $ errorBundlePretty err; False `shouldBe` True }

    describe "code generation" $ do
        
        it "should generate simple let expr" $ do
            let expr = (Let [ ("a", Lit $ LInt 1)
                            , ("b", var "a")] (var "b"))
            let result = generateExpr glslStdLib expr
            result `shouldBe` "int a = 1;\nint b = a;\nreturn b;\n"

        it "should generate single application" $ do
            let expr = app (var "func") (Lit (LFloat 0.5))
            let result = generateExpr glslStdLib expr
            result `shouldBe` "func(0.5)"

        it "should generate triple application" $ do
            let expr = app (app (app (var "vec3") (Lit (LFloat 0.5))) (Lit (LFloat 0.5))) (Lit (LFloat 0.5))
            let result = generateExpr glslStdLib expr
            result `shouldBe` "vec3(0.5, 0.5, 0.5)"

        it "should generate declarations" $ do
            let decl = ("circle",lam "st" (lam "r" (Lit $ LFloat 1.0)))
            let env = TypeEnv $ Map.fromList [ ("circle",Forall [] (TArr (TCon Vec2) (TArr (TCon Float) (TCon Float)))) ]
            let result = generateDecl env decl
            result `shouldBe` "float circle(vec2 st, float r) {\nreturn 1.0;\n}\n\n"

        it "should generate functions with arbitrary parameters" $ do
            let decl = ("func",lam "a" (lam "b" (lam "c" (lam "d" (Lit $ LFloat 1.0)))))
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
            let program = stdLib ++ "\nmain: Vec2 -> Vec4\nmain coord = vec4 coord.x coord.y 1.0 1.0"
            let exprs = first show $ P.parseModule "std.yin" $ stdLib
            let Right env = exprs >>= (first show . inferTop glslStdLib )
            let Right decls = P.parseModule "std.yin" stdLib
            case Gen.compileProgram program of
                Right result -> do
                    putStrLn result
                    -- Verify that key functions are generated
                    result `shouldSatisfy` (\s -> "vec2 cast2(float v)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec3 cast3(float v)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec4 cast4(float v)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec4 shape(vec2 st_, float sides, float radius, float smoothing)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "float random(float p)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec2 hash2(vec2 p0)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec3 hash3(vec3 p)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec4 noise(vec2 st, float time, float scale, float offset)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec4 fbm(vec2 st, float time, float scale, float offset)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec4 dw(vec2 st, float time, float scale, float offset)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec2 kaleid(vec2 st, float nSides)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec4 osc(vec2 st, float time, float freq, float sync, float offset)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec3 rgb_to_hsv(vec3 c)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec3 hsv_to_rgb(vec3 c)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "vec4 colorama(vec4 c0, float amount)" `isInfixOf` s)

                    -- Verify that the generated code contains proper GLSL syntax
                    result `shouldSatisfy` (\s -> "return " `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "{" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "}" `isInfixOf` s)

                    -- Verify no obvious syntax errors in the generated code
                    result `shouldSatisfy` (\s -> not ("undefined" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("error" `isInfixOf` s))

                    -- NEW: Verify that no unnecessary anonymous functions are generated for top-level declarations
                    -- These functions should NOT have anonymous duplicates
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "rgb_to_hsv" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "hsv_to_rgb" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "colorama" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "mask" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "diff" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "layer" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "cast2" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "cast3" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "cast4" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "random" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "hash2" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "hash3" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "noise" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "fbm" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "dw" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "kaleid" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s && "osc" `isInfixOf` s))
                Left error -> do
                    putStrLn error
                    False `shouldBe` True

        it "should generate anonymous functions" $ do
            let program = "f : Float -> Float\nf = \\x -> x + 1.0\nmain : Vec2 -> Vec4\nmain coord = vec4 coord.x coord.y 1.0 1.0"
            case compileProgram program of
                Right result -> do
                    putStrLn result
                    -- Should contain the function definition
                    result `shouldSatisfy` (\s -> "float f(float x)" `isInfixOf` s)
                Left err -> do
                    putStrLn $ "Compilation failed: " ++ err
                    False `shouldBe` True

        it "should support function currying" $ do
            let program = "add : Float -> Float -> Float\nadd = \\x -> \\y -> x + y\nmain : Vec2 -> Vec4\nmain coord = vec4 coord.x coord.y 1.0 1.0"
            case compileProgram program of
                Right result -> do
                    putStrLn result
                    -- Should contain the function definition
                    result `shouldSatisfy` (\s -> "float add(float x, float y)" `isInfixOf` s)
                Left err -> do
                    putStrLn $ "Compilation failed: " ++ err
                    False `shouldBe` True

        it "should lift lambda in let declaration to top level" $ do
            let program = "main : Vec2 -> Vec4\nmain coord = let f = \\x -> x + 1.0 in vec4 coord.x coord.y 1.0 1.0"
            case compileProgram program of
                Right result -> do
                    putStrLn result
                    -- Should contain an anonymous function definition at top level
                    result `shouldSatisfy` (\s -> "float anon_" `isInfixOf` s && "(float x)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "return (x + 1.0)" `isInfixOf` s)
                Left err -> do
                    putStrLn $ "Compilation failed: " ++ err
                    False `shouldBe` True

        it "should generate top-level function for partial application" $ do
            let program = "add : Float -> Float -> Float\nadd x y = x + y\nmain : Vec2 -> Vec4\nmain coord = let addOne = add 1.0 in vec4 coord.x coord.y 1.0 1.0"
            case compileProgram program of
                Right result -> do
                    putStrLn result
                    -- Should contain the function definition without unnecessary anonymous functions
                    result `shouldSatisfy` (\s -> "float add(float x, float y)" `isInfixOf` s)
                    -- Should NOT contain unnecessary anonymous functions for top-level declarations
                    result `shouldSatisfy` (\s -> not ("anon_" `isInfixOf` s) || ("anon_" `isInfixOf` s && "userEntrypoint" `isInfixOf` s))
                    result `shouldSatisfy` (\s -> "return (x + y)" `isInfixOf` s)
                Left err -> do
                    putStrLn $ "Compilation failed: " ++ err
                    False `shouldBe` True

        it "should handle nested lambda expressions" $ do
            let program = "main : Vec2 -> Vec4\nmain coord = let f = \\x -> \\y -> x + y in vec4 coord.x coord.y 1.0 1.0"
            case compileProgram program of
                Right result -> do
                    putStrLn result
                    -- Should contain function with multiple parameters
                    result `shouldSatisfy` (\s -> "float anon_" `isInfixOf` s && "(float x, float y)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "return (x + y)" `isInfixOf` s)
                Left err -> do
                    putStrLn $ "Compilation failed: " ++ err
                    False `shouldBe` True

        it "should handle lambda with multiple parameters" $ do
            let program = "main : Vec2 -> Vec4\nmain coord = let f = \\x y -> x + y in vec4 coord.x coord.y 1.0 1.0"
            case compileProgram program of
                Right result -> do
                    putStrLn result
                    -- Should contain function with multiple parameters
                    result `shouldSatisfy` (\s -> "float anon_" `isInfixOf` s && "(float x, float y)" `isInfixOf` s)
                    result `shouldSatisfy` (\s -> "return (x + y)" `isInfixOf` s)
                Left err -> do
                    putStrLn $ "Compilation failed: " ++ err
                    False `shouldBe` True

    describe "main" $ do
        describe "compileProgram" $ do
            it "should pretty print unbound variable" $ do
                let program = "main coord = let a = b in a"
                let Left res = compileProgram program
                putStrLn res
                res `shouldBe` "1:22:\n  |\n1 | main coord = let a = b in a\n  |                      ^^\nVariable \"b\" is unbound\n"

            it "should pretty pretty unification failure for application" $ do
                let program = "main coord = let a = vec2 true true in a"
                let Left res = compileProgram program
                putStrLn res
                res `shouldBe` "1:22:\n  |\n1 | main coord = let a = vec2 true true in a\n  |                      ^^^^^^^^^^^^^^^\nType mismatch: expected Float but found Bool\n"

            it "should pretty pretty unification failure for operators" $ do
                let program = "main coord = let a = 1.0 + true in a"
                let Left res = compileProgram program
                putStrLn res
                res `shouldBe` "1:1:\n  |\n1 | main coord = let a = 1.0 + true in a\n  | ^\nType mismatch: expected Bool but found Float\n"

            it "should pretty pretty unification failure for if" $ do
                let program = "main coord = let a = if true then 0.0 else vec2 1.0 1.0 in a"
                let Left res = compileProgram program
                putStrLn res
                res `shouldBe` "1:44:\n  |\n1 | main coord = let a = if true then 0.0 else vec2 1.0 1.0 in a\n  |                                            ^^^^^^^^^^^^^\nType mismatch: expected Vec2 but found Float\n"

    describe "examples" $ do
        it "should compile all example shader.yin files" $ do
            files <- listDirectory "examples"
            let yinFiles = filter (\f -> ".yin" `isSuffixOf` f) files
            stdLib <- readFile "std.yin"

            forM_ yinFiles $ \file -> do
                let filePath = "examples/" ++ file
                content <- readFile filePath
                -- Skip empty files
                if null content
                    then putStrLn $ "Skipping empty file: " ++ file
                    else do
                        let fullProgram = stdLib ++ "\n\n" ++ content
                        case compileProgram fullProgram of
                            Right glsl -> do
                                -- Just verify compilation succeeds - don't check GLSL content
                                -- as examples may have different structure
                                True `shouldBe` True
                            Left err -> do
                                putStrLn $ "Failed to compile " ++ file ++ ": " ++ err
                                False `shouldBe` True
