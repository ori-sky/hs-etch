{-# LANGUAGE OverloadedStrings #-}

module Etch.CodeGen where

import Data.Foldable (traverse_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as ShortBS
import Control.Monad.State
import qualified LLVM as L
import qualified LLVM.Context as L
import qualified LLVM.AST as L.AST
import qualified LLVM.AST.Global as L.AST
import qualified LLVM.AST.Type as L.AST (i32)
import Etch.AST

codeGen :: [AST] -> IO ()
codeGen asts = L.withContext $ \context -> do
    L.withModuleFromAST context astM $ \m -> do
        ll <- L.moduleLLVMAssembly m
        putStrLn (BS.unpack ll)
  where astM = execState (traverse_ astGen asts) L.AST.defaultModule

astGen :: AST -> State L.AST.Module ()
astGen (Call "=" (Tuple [ Identifier name
                        , Function (Tuple args) (Block asts)
                        ])) =
    addDefinition $ L.AST.GlobalDefinition $ L.AST.functionDefaults
        { L.AST.name       = L.AST.Name (ShortBS.toShort name)
        , L.AST.returnType = L.AST.i32
        }
astGen (Call "=" (Tuple [ Identifier name
                        , Function arg (Block asts)
                        ])) =
    addDefinition $ L.AST.GlobalDefinition $ L.AST.functionDefaults
        { L.AST.name       = L.AST.Name (ShortBS.toShort name)
        , L.AST.returnType = L.AST.i32
        }

addDefinition :: L.AST.Definition -> State L.AST.Module ()
addDefinition def = do
    defs <- gets L.AST.moduleDefinitions
    modify $ \s -> s { L.AST.moduleDefinitions = defs ++ [def] }

