import Data.Bifunctor
import System.Directory
import Test.Hspec

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

        it "should parse complex exprs" $ do
            let expr = Let [ ("b", Lit $ LInt 2)
                           , ("c", Lit $ LInt 3)
                           , ("a", If (Lit $ LBool True) 
                                     (Op Sub (Var "b") (Var "b")) 
                                     (Op Add (Var "c") (Var "c")))
                           ]
                        (Op Mul (Var "a") (Var "c"))
            let res = inferExpr mempty expr
            res `shouldBe` Right (Forall [] typeInt)

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

        it "should succeed" $ do
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

        it "should generate application" $ do
            let expr = App (App (App (Var "vec3") (Lit (LFloat 0.5))) (Lit (LFloat 0.5))) (Lit (LFloat 0.5))
            let result = generateExpr glslStdLib expr
            result `shouldBe` "vec3(0.5, 0.5, 0.5)"
