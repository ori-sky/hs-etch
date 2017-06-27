module Main where

import Data.ByteString.Char8 (hGetContents)
import Text.Show.Pretty (pPrint)
import System.IO (IOMode(ReadMode), stdin, openBinaryFile)
import System.Environment (getArgs)
import qualified Etch.Parser as P
import qualified Etch.CodeGen as CG

main :: IO ()
main = do
    (f:_) <- getArgs
    handle <- case f of
        "-"  -> pure stdin
        path -> openBinaryFile path ReadMode
    eASTs <- P.parse <$> hGetContents handle
    pPrint eASTs
    case eASTs of
        Left str   -> putStrLn ("failed to parse: " ++ str)
        Right asts -> CG.codeGen asts
