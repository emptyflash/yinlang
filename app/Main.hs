module Main where

import System.Directory
import System.Exit
import System.IO


import Type
import Syntax

import qualified Gen as Gen


main :: IO ()
main = do
    program <- getContents
    path <- makeAbsolute "std.yin"
    stdLib <- readFile path
    let completeProg = stdLib ++ "\n\n" ++ program
    let result = Gen.compileProgram completeProg
    case result of
        Right code -> do
            putStrLn code
            exitSuccess
        err -> do
            hPutStrLn stderr $ show err
            exitFailure
