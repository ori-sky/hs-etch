{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text.IO (hGetContents)
import Control.Monad.Except
import Control.Monad.State (runStateT)
import Text.Show.Pretty (pPrint)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), Handle, stdin, openBinaryFile)
import Etch.Analysis
import qualified Etch.Semantics as Semantics
import qualified Etch.Analysis.Resolution as Resolution
import Etch.Parser (parse)
--import Etch.CodeGen (codeGen)
import Etch.Types.ErrorContext
--import Etch.Types.Module (defaultModule)
import Etch.Types.Semantic

main :: IO ()
main = do
    args <- getArgs
    contents <- hGetContents =<< getHandle args
    --print contents
    runExceptT (compile (getSrcFile args) contents) >>= \case
        Left (ErrorContext err contexts) -> putStrLn (intercalate "\n\n" (err : contexts))
        Right code                       -> putStrLn code

compile :: (MonadError ErrorContext m, MonadIO m) => String -> Text -> m String
compile _ contents = do
    p <- parse contents
    liftIO (pPrint p)
    (sem, state) <- runStateT (Semantics.analysis p) defaultAnalysisState
    liftIO (pPrint state)
    liftIO (pPrint sem)
    (res, st) <- runResolution =<< runResolution =<< runResolution =<< runResolution =<< runResolution =<< runResolution (sem, state)
    liftIO (pPrint st)
    liftIO (pPrint res)
    pure ""
    --let defs = [ def | DefStatement def `As` _ <- sem ]
    --liftIO $ codeGen (defaultModule name defs)

runResolution :: (MonadError ErrorContext m) => ([Typed SemBox], AnalysisState) -> m ([Typed SemBox], AnalysisState)
runResolution (sems, state) = runStateT (runAnalysis Resolution.analysis sems) state

getHandle :: [String] -> IO Handle
getHandle ("-"  : _) = pure stdin
getHandle (path : _) = openBinaryFile path ReadMode
getHandle []         = pure stdin

getSrcFile :: [String] -> String
getSrcFile ("-"  : _) = "a.e"
getSrcFile (path : _) = path
getSrcFile []         = "a.e"
