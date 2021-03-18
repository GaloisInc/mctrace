module Main ( main ) where

import qualified Control.Exception as X
import qualified Data.Aeson as DA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Functor.Const as C
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Text.IO as TIO
import qualified LLVM.CodeGenOpt as LLCGO
import qualified LLVM.CodeModel as LLC
import qualified LLVM.Context as LLCX
import qualified LLVM.Module as LLM
import qualified LLVM.Relocation as LLR
import qualified LLVM.Target as LLT
import qualified Options.Applicative as O

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT

import qualified MCTrace.Codegen.LLVM as MCL

data Options =
  Options { dtraceFile :: FilePath
          , llvmAsmFile :: Maybe FilePath
          , asmFile :: Maybe FilePath
          , objFile :: Maybe FilePath
          , saveVarMapping :: Maybe FilePath
          }

options :: O.ParserInfo Options
options = O.info (O.helper <*> parser)
          ( O.fullDesc
          <> O.progDesc "Instrument a binary with trace points"
          )
  where
    parser = Options <$> O.strOption
                          ( O.long "script"
                          <> O.metavar "FILE"
                          <> O.help "A DTrace script defining the probes to inject into the binary"
                          )
                     <*> O.optional (O.strOption
                           ( O.long "save-llvm-asm"
                           <> O.metavar "FILE"
                           <> O.help "A file to save generated LLVM assembly to"
                           ))
                     <*> O.optional (O.strOption
                           ( O.long "save-asm"
                           <> O.metavar "FILE"
                           <> O.help "A file to save generated machine assembly to"
                           ))
                     <*> O.optional (O.strOption
                           ( O.long "save-obj"
                           <> O.metavar "FILE"
                           <> O.help "A file to save generated object file to"
                           ))
                     <*> O.optional (O.strOption
                           ( O.long "save-var-mapping"
                           <> O.metavar "FILE"
                           <> O.help "Save the mapping of variable names to offsets into storage for global variables"
                           ))

data TraceException = DTraceParseFailure FilePath X.SomeException
                    | DTraceTypeCheckFailure FilePath [LD.TypeError]
  deriving (Show)

instance X.Exception TraceException

loadProbes :: Options -> IO LDT.Probes
loadProbes opts = do
  let scriptFile = dtraceFile opts
  scriptText <- TIO.readFile scriptFile
  case LD.parseDTrace scriptFile scriptText of
    Left err -> X.throwIO (DTraceParseFailure scriptFile err)
    Right decls ->
      case LD.typeCheck decls of
        Left errs -> X.throwIO (DTraceTypeCheckFailure scriptFile errs)
        Right probes -> return probes

withOptional :: Maybe a -> (a -> IO ()) -> IO ()
withOptional ma k =
  case ma of
    Nothing -> return ()
    Just a -> k a

main :: IO ()
main = do
  opts <- O.execParser options
  probes <- loadProbes opts

  case MCL.compileProbesLLVM "dtraceProbes" probes of
    MCL.CompiledProbes gvars gvarOffsets _probes llvmModule -> do
      LLT.initializeAllTargets
      LLCX.withContext $ \ctx -> LLM.withModuleFromAST ctx llvmModule $ \nativeModule -> do
        -- FIXME: Once we accept an ELF file, we'll need to use the general
        -- 'withTargetMachine' for cross compilation
        LLT.withHostTargetMachine LLR.Default LLC.Default LLCGO.Aggressive $ \tm -> do
          withOptional (llvmAsmFile opts) $ \path -> do
              bs <- LLM.moduleLLVMAssembly nativeModule
              BS.writeFile path bs
          withOptional (asmFile opts) $ \path -> do
              bs <- LLM.moduleTargetAssembly tm nativeModule
              BS.writeFile path bs
          withOptional (objFile opts) $ \path -> do
              bs <- LLM.moduleObject tm nativeModule
              BS.writeFile path bs
          withOptional (saveVarMapping opts) $ \path -> do
            let extractVar acc idx = (LDT.globalVarName (gvars Ctx.! idx), C.getConst (gvarOffsets Ctx.! idx)) : acc
            let bs = DA.encode (Map.fromList (Ctx.forIndex (Ctx.size gvars) extractVar []))
            BSL.writeFile path bs
