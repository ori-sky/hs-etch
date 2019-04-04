{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text.IO (hGetContents)
import Control.Monad.Except
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Text.Show.Pretty (pPrint)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), Handle, stdin, openBinaryFile)
import qualified Etch.Analysis.Semantics as Semantics
import qualified Etch.Analysis.Resolution as Resolution
import Etch.Parser (parse)
import Etch.CodeGen (codeGen)
import Etch.Types.Analysis
import Etch.Types.ErrorContext
import Etch.Types.Module (defaultModule)
import Etch.Types.SemanticTree (Statement(DefStatement), Typed(As))

main :: IO ()
main = do
    args <- getArgs
    contents <- hGetContents =<< getHandle args
    --print contents
    runExceptT (compile (getSrcFile args) contents) >>= \case
        Left (ErrorContext err contexts) -> putStrLn (intercalate "\n\n" (err : contexts))
        Right code                       -> putStrLn code

compile :: (MonadError ErrorContext m, MonadIO m) => String -> Text -> m String
compile name contents = do
    p <- parse contents
    liftIO (pPrint p)
    (sem, state) <- runStateT (Semantics.analysis p) defaultAnalysisState
    liftIO (pPrint sem)
    liftIO (pPrint state)
    res <- runReaderT (Resolution.analysis sem) state
    liftIO (pPrint res)
    let defs = [ def | DefStatement def `As` _ <- res ]
    liftIO $ codeGen (defaultModule name defs)

getHandle :: [String] -> IO Handle
getHandle ("-"  : _) = pure stdin
getHandle (path : _) = openBinaryFile path ReadMode
getHandle []         = pure stdin

getSrcFile :: [String] -> String
getSrcFile ("-"  : _) = "a.e"
getSrcFile (path : _) = path
getSrcFile []         = "a.e"
