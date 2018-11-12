{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO (IOMode(ReadMode), Handle, stdin, openBinaryFile)
import Data.Text.IO (hGetContents)
import System.Environment (getArgs)
import qualified Etch.Parser as P
import qualified Etch.CodeGen as CG
import qualified Etch.Module as M

main :: IO ()
main = do
    args <- getArgs
    handle <- getHandle args
    let srcFile = getSrcFile args
    P.parse <$> hGetContents handle >>= \case
        Left str   -> putStrLn ("failed to parse: " ++ str)
        Right defs -> putStrLn =<< CG.codeGen (M.defaultModule srcFile defs)

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
