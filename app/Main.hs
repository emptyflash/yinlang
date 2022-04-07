{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import Data.FileEmbed
import Data.ByteString.Char8 (unpack)

import Type
import Syntax

import qualified Gen as Gen


getStdLib :: IO String
getStdLib = do
    args <- getArgs
    case args of
        ["-l", path] -> readFile path
        _ -> pure $ unpack $(embedFile "./std.yin")
    

main :: IO ()
main = do
    program <- getContents
    args <- getArgs
    stdLib <- getStdLib
    let completeProg = stdLib ++ "\n\n" ++ program
    let result = Gen.compileProgram completeProg
    case result of
        Right code -> do
            putStrLn code
            exitSuccess
        Left err -> do
            hPutStrLn stderr err
            exitFailure
