module Main where

import Data.ByteString.Char8 (hGetContents)
import Text.Show.Pretty (pPrint)
import System.IO (IOMode(ReadMode), stdin, openBinaryFile)
import System.Environment (getArgs)
import Etch.Parser as Parser

main :: IO ()
main = do
    (f:_) <- getArgs
    handle <- case f of
        "-"  -> pure stdin
        path -> openBinaryFile path ReadMode
    pPrint . Parser.parse =<< hGetContents handle
