import qualified Data.Text.Lazy as L
import qualified Parser as P
import Text.Parsec as PA

import System.Directory
import Test.Hspec
import Syntax
import Lexer
import Debug.Trace


main :: IO ()
main = hspec $ do
    describe "parser" $ do
        it "should parse addition" $ do
            let expr = "1 + 1"
            let result = P.parseExpr (L.pack expr)
            result `shouldBe` Right (Op Add (Lit $ LInt 1) (Lit $ LInt 1))

        it "should parse multiplication" $ do
            let expr = "1 * 1"
            let result = P.parseExpr (L.pack expr)
            result `shouldBe` Right (Op Mul (Lit $ LInt 1) (Lit $ LInt 1))

        it "should parse swizzle" $ do
            let expr = "a.xyz"
            let result = P.parseExpr (L.pack expr)
            result `shouldBe` Right (Op Swizzle (Var "a") (Var "xyz"))

        it "should parse nested application" $ do
            let expr = "a 1 1"
            let result = P.parseExpr (L.pack expr)
            result `shouldBe` Right (App (App (Var "a") (Lit $ LInt 1)) (Lit $ LInt 1))

        it "should parse simple let expr" $ do
            let expr = "let a = 1 in a"
            let result = P.parseExpr (L.pack expr)
            result `shouldBe` Right (Let [("a", Lit $ LInt 1)] (Var "a"))

        it "should parse multi let expr" $ do
            let expr = "let a = 1\nb = 2\n in a"
            let result = P.parseExpr (L.pack expr)
            result `shouldBe` Right (Let [("a", Lit $ LInt 1),
                                          ("b", Lit $ LInt 2)] (Var "a"))

        it "should parse complex exprs" $ do
            let expr = "let a = if true then b - b else c + c in a * c"
            let result = P.parseExpr (L.pack expr)
            result `shouldBe` Right (Let [("a", If (Lit $ LBool True) (Op Sub (Var "b") (Var "b")) (Op Add (Var "c") (Var "c")))] (Op Mul (Var "a") (Var "c")))

        it "should succeed" $ do
            path <- makeAbsolute "std.yin"
            stdLib <- readFile path
            case P.parseModule "std.yin" (L.pack stdLib) of
                Right a -> do { putStrLn $ show a; True `shouldBe` True }
                Left err -> do { putStrLn $ show err; False `shouldBe` True }
