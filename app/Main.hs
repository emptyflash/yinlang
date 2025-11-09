{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import Data.FileEmbed
import Data.ByteString.Char8 (unpack)
import Control.Monad

import Type
import Syntax

import qualified Gen as Gen


data Config = Config
    { stdLibPath :: Maybe FilePath
    , inputPath :: FilePath
    , outputPath :: FilePath
    }

parseArgs :: [String] -> Either String Config
parseArgs args = parse args (Config Nothing "-" "-")
  where
    parse [] config = Right config
    parse ("-l":stdPath:rest) config = parse rest (config { stdLibPath = Just stdPath })
    parse ("-o":output:rest) config = parse rest (config { outputPath = output })
    parse [input] config = Right $ config { inputPath = input }
    parse [] config = Right config
    parse _ _ = Left "Usage: yin [-l stdlib.yin] [-o output.hs] input.yin\n  Use '-' for stdin/stdout"

getStdLib :: Maybe FilePath -> IO String
getStdLib stdLibPath = case stdLibPath of
    Just path -> readFile path
    Nothing -> pure $ unpack $(embedFile "./std.yin")

readInput :: FilePath -> IO String
readInput "-" = getContents
readInput path = readFile path

writeOutput :: FilePath -> String -> IO ()
writeOutput "-" = putStrLn
writeOutput path = writeFile path

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> do
            hPutStrLn stderr err
            exitFailure
        Right config -> do
            program <- readInput (inputPath config)
            stdLib <- getStdLib (stdLibPath config)
            let completeProg = stdLib ++ "\n\n" ++ program
            putStrLn completeProg
            let result = Gen.compileProgram completeProg
            case result of
                Right code -> do
                    writeOutput (outputPath config) code
                    exitSuccess
                Left err -> do
                    hPutStrLn stderr err
                    exitFailure
