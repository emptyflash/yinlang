module Main where

import Data.Bifunctor
import System.Directory
import System.Exit
import System.IO

import Type
import Syntax

import qualified Data.Map as Map

import qualified Parser as Parser
import qualified Infer as Infer
import qualified Gen as Gen


renameMapKey :: Ord a => a -> a -> Map.Map a b -> Map.Map a b
renameMapKey old new m =
    case Map.lookup old m of
        Nothing -> m
        Just v -> Map.insert new v $ Map.delete old m

renameMainType :: Infer.TypeEnv -> Infer.TypeEnv
renameMainType (Infer.TypeEnv env) = Infer.TypeEnv $ renameMapKey "main" "userEntrypoint" env

renameMain :: [Decl] -> [Decl]
renameMain (("main", expr) : xs) = ("userEntrypoint", expr) : xs
renameMain (x : xs) = x : renameMain xs
renameMain [] = []

compileProgram :: String -> Either String String
compileProgram prog = do
    decls <- first show $ Parser.parseModule "<stdin>" prog
    env <- first show $ Infer.inferTop Infer.glslStdLib decls
    newEnv <- case Infer.typeof env "main" of
        Just (Forall [] (TCon Vec2 `TArr` TCon Vec4)) -> Right $ renameMainType env
        Just scheme -> Left $ "Missing main function with correct type. Expected: Vec2 -> Vec4, Found: " ++ show scheme
    let newDecls = renameMain decls
    let code = newDecls >>= Gen.generateDecl newEnv 
    pure $ code ++ "\n\nvoid main() { gl_FragColor = userEntrypoint(gl_FragCoord.xy); }"

main :: IO ()
main = do
    program <- getContents
    path <- makeAbsolute "std.yin"
    stdLib <- readFile path
    let completeProg = stdLib ++ "\n\n" ++ program
    let result = compileProgram completeProg
    case result of
        Right code -> do
            putStrLn code
            exitSuccess
        err -> do
            hPutStrLn stderr $ show err
            exitFailure
