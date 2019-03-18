{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text.IO (hGetContents)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), Handle, stdin, openBinaryFile)
import qualified Etch.Analysis.Semantics as Semantics
import Etch.Parser (parse)
import Etch.CodeGen (codeGen)
import Etch.Types.Module (defaultModule)
import Etch.Types.SemanticTree (Statement(DefStatement), Typed(As))

main :: IO ()
main = do
    args <- getArgs
    contents <- hGetContents =<< getHandle args
    case parse contents >>= Semantics.analysis of
        Left str         -> putStrLn ("analysis failed: " ++ str)
        Right statements -> do
            putStrLn =<< codeGen (defaultModule (getSrcFile args) defs)
          where defs = [ def | DefStatement def `As` _ <- statements ]

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
