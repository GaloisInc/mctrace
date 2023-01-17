{-# LANGUAGE RankNTypes #-}
module MCTrace.Builtins (
    BuiltinVarCompilerArgs(..),
    BuiltinVarCompiler(..),
    BuiltinVarCompilerMap,
    builtinVarCompilers
) where

import qualified Data.Map as Map

import qualified Language.DTrace.Syntax.Typed as ST
import qualified LLVM.AST as IR
import qualified LLVM.AST.Type as IRT
import qualified LLVM.IRBuilder as IRB

import qualified MCTrace.Runtime as RT

data BuiltinVarCompilerArgs = 
  BuiltinVarCompilerArgs { supportFunctions :: IR.Operand
                         , extraArg :: Maybe IR.Operand
                         }

data BuiltinVarCompiler = 
  BuiltinVarCompiler { compile :: forall m. (IRB.MonadModuleBuilder m, IRB.MonadIRBuilder m) => BuiltinVarCompilerArgs -> m IR.Operand
                     , argType :: Maybe IR.Type
                     , requireRewrite :: Bool
                     }  

type BuiltinVarCompilerMap = Map.Map ST.Builtin BuiltinVarCompiler



builtinVarCompilers :: Map.Map ST.Builtin BuiltinVarCompiler
builtinVarCompilers = Map.fromList [ (ST.Timestamp, timestampBuiltinCompiler)
                                ]


timestampBuiltinCompiler :: BuiltinVarCompiler
timestampBuiltinCompiler = BuiltinVarCompiler { compile = compiler 
                                              , argType = Nothing
                                              , requireRewrite = False
                                              }
  where
    compiler :: (IRB.MonadModuleBuilder m, IRB.MonadIRBuilder m) => BuiltinVarCompilerArgs -> m IR.Operand
    compiler BuiltinVarCompilerArgs { supportFunctions = fnOperand } = do
        fnAddr <- IRB.gep fnOperand [IRB.int32 timestampFnIndex]
        fn <- IRB.load fnAddr 0
        castedFn <- IRB.bitcast fn (IRT.ptr timestampFnType)
        IRB.call castedFn []
    timestampFnIndex = fromIntegral $ RT.probeSupportFunctionIndexMap Map.! RT.Timestamp
    timestampFnType = IRT.FunctionType IRT.i64 [] False   
      