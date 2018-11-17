{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO (IOMode(ReadMode), Handle, stdin, openBinaryFile)
import Data.Text.IO (hGetContents)
import System.Environment (getArgs)
import Etch.Parser (parse)
import Etch.CodeGen (codeGen)
import Etch.Types.Module (defaultModule)

main :: IO ()
main = do
    args <- getArgs
    handle <- getHandle args
    let srcFile = getSrcFile args
    parse <$> hGetContents handle >>= \case
        Left str   -> putStrLn ("failed to parse: " ++ str)
        Right defs -> putStrLn =<< codeGen (defaultModule srcFile defs)

getHandle :: [String] -> IO Handle
getHandle = \case
    ("-"  : _) -> pure stdin
    (path : _) -> openBinaryFile path ReadMode
    []         -> pure stdin

getSrcFile :: [String] -> String
getSrcFile = \case
    ("-"  : _) -> "a.e"
    (path : _) -> path
    []         -> "a.e"
