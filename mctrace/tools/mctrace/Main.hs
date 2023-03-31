{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import qualified Control.Exception as X
import qualified Data.Aeson as DA
import qualified Data.Binary.Get as DBG
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ElfEdit as DE
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import qualified Data.Traversable as T
import           Data.Word ( Word32, Word64 )
import qualified LLVM.Analysis as LLA
import qualified LLVM.Context as LLCX
import qualified LLVM.Module as LLM
import qualified LLVM.Target as LLT
import qualified Lumberjack as LJ
import qualified Options.Applicative as OA
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PPT
import qualified System.Directory as SD
import qualified System.Exit as SE
import qualified System.IO as SI

import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import           Data.Macaw.X86.Symbolic ()
import qualified Lang.Crucible.FunctionHandle as LCF
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as RX
import qualified Renovate.Arch.PPC as RP

import qualified Language.DTrace.Syntax.Typed as LDT
import qualified MCTrace.Analysis as MA
import qualified MCTrace.Arch.X86 as MRAX
import qualified MCTrace.Arch.PPC as MRAP
import qualified MCTrace.Codegen as MC
import qualified MCTrace.Codegen.LLVM as MCL
import qualified MCTrace.Exceptions as ME
import qualified MCTrace.Loader as ML
import qualified MCTrace.Panic as MP

import qualified Debug.Trace as Trace

import qualified Options as O

withOptional :: Maybe a -> (a -> IO ()) -> IO ()
withOptional ma k =
  case ma of
    Nothing -> return ()
    Just a -> k a


-- | The action to run for each supported architecture
--
-- This tool both analyzes binaries and rewrites them, so it needs to request
-- all supported features from renovate using the 'R.AnalyzeAndRewrite' type
-- (which is partially applied here)
--
-- FIXME: Move this to a new module, also split out the code to index probes to
-- their offsets in the text section of the probe file (and also handle the
-- validation of that file)
archConfigurations
  :: MC.ProbeIndex globals w
  -> DE.ElfHeaderInfo n
  -> [(R.Architecture, R.SomeConfig (R.AnalyzeAndRewrite MA.LogEvent) (MA.ProbeLocationAnalysisResult globals))]
archConfigurations probes library =
  [ (R.X86_64, R.SomeConfig (PN.knownNat @64) MBL.Elf64Repr (RX.config (MRAX.x86Rewriter probes library)))
  , (R.PPC32, R.SomeConfig (PN.knownNat @32) MBL.Elf32Repr (RP.config32 (MRAP.ppcRewriter probes library)))
  ]

saveLLVMDebugInfo
  :: O.IOptions
  -> LLT.TargetMachine
  -> LLM.Module
  -> BS.ByteString
  -> IO ()
saveLLVMDebugInfo iopts tm nativeModule objBytes = do
  withOptional (O.iLLVMAsmFile iopts) $ \path -> do
      bs <- LLM.moduleLLVMAssembly nativeModule
      BS.writeFile path bs
  withOptional (O.iAsmFile iopts) $ \path -> do
      bs <- LLM.moduleTargetAssembly tm nativeModule
      BS.writeFile path bs
  withOptional (O.iObjFile iopts) $ \path -> do
      BS.writeFile path objBytes

globalsSize :: Ctx.Assignment LDT.GlobalVariable globals
            -> Ctx.Assignment (C.Const Word32) globals
            -> Int
globalsSize _varDefs varOffsets = 8 * Ctx.sizeInt (Ctx.size varOffsets)

renovateLogger :: (PP.Pretty l) => LJ.LogAction IO (R.Diagnostic l)
renovateLogger = LJ.LogAction $ \diag -> do
  SI.hPutStrLn SI.stderr (show (PP.pretty diag))

instrument :: O.IOptions -> IO ()
instrument iopts = do
  probes <- ML.loadProbes (O.iDTraceFile iopts)
  someHeader <- ML.loadBinary (O.iInputExecutableFile iopts)
  case MCL.compileProbesLLVM "dtraceProbes" probes of
    MCL.CompiledProbes gvars gvarOffsets namedProbes llvmModule -> do
      LLT.initializeAllTargets
      MCL.withLLVMOptions someHeader $ \tm -> do
        LLCX.withContext $ \ctx -> LLM.withModuleFromAST ctx llvmModule $ \nativeModule -> do
          -- Verify this module to catch errors early
          LLA.verify nativeModule
          -- Construct the object file containing the probe definitions from our
          -- LLVM IR; this must be invoked before the rewriter, as the probes
          -- are a required argument to the rewriter
          objBytes <- LLM.moduleObject tm nativeModule
          saveLLVMDebugInfo iopts tm nativeModule objBytes

          -- Write out the required metadata
          let extractVar acc idx = (LDT.globalVarName (gvars Ctx.! idx), C.getConst (gvarOffsets Ctx.! idx)) : acc
          let mappingFileBytes = DA.encode (Map.fromList (Ctx.forIndex (Ctx.size gvars) extractVar []))
          BSL.writeFile (O.iVarMappingFile iopts) mappingFileBytes

          DE.SomeElf mcProbeELF <- ML.loadGeneratedProbes objBytes
          -- file, bytes
          let storageFile = O.iPersistenceFile iopts
          let storageSize = globalsSize gvars gvarOffsets
          probeIndex <- case MC.indexELFProbes namedProbes mcProbeELF storageFile (fromIntegral storageSize) of
            Left err -> X.throwIO err
            Right idx -> return idx

          -- Load the runtime support library module
          DE.SomeElf mcLibraryELF <- ML.loadBinary (O.iLibraryFile iopts)

          hdlAlloc <- LCF.newHandleAllocator

          -- Use the binary rewriter to insert the generated code and add calls
          -- to the probes at appropriate locations
          let configs = archConfigurations probeIndex mcLibraryELF
          R.withElfConfig someHeader configs $ \renovateConfig headerInfo loadedBinary -> do
            let strat = R.LayoutStrategy R.Parallel R.BlockGrouping R.AlwaysTrampoline
            (newElf0, ares, rwInfo) <- R.rewriteElf renovateLogger renovateConfig hdlAlloc headerInfo loadedBinary strat
            let symbolicEntryAddr = MA.injectedEntryAddr (MA.injectedAssets ares)
            let redirRes = R.riRedirectionResult rwInfo
            case Map.lookup symbolicEntryAddr (R.rrSymbolicToConcreteMap redirRes) of
              Nothing -> MP.panic MP.ELFRewriter "instrument" ["No new entry point address allocated"]
              Just concEntryAddr -> do
                let newElf1 = newElf0 { DE.elfEntry = fromIntegral (R.absoluteAddress concEntryAddr) }
                let exeFile = O.iOutputExecutableFile iopts
                BSL.writeFile exeFile (DE.renderElf newElf1)
                p0 <- SD.getPermissions exeFile
                SD.setPermissions exeFile (SD.setOwnerExecutable True p0)
                putStrLn "Added instrumentation in blocks:"
                F.forM_ (R.riRewritePairs rwInfo) $ \(R.RewritePair origBlock mNew) -> do
                  case mNew of
                    Nothing -> return ()
                    Just _newBlock -> putStrLn ("  " ++ show (R.concreteBlockAddress origBlock))
                return ()

readMappingFile
  :: FilePath
  -> IO (Map.Map String Word32)
readMappingFile p = do
  dataBytes <- BSL.readFile p
  case DA.eitherDecode dataBytes of
    Left err -> X.throwIO (ME.ErrorReadingMappingFile p err)
    Right m -> return m

decodePersistedOffset
  :: FilePath
  -> BSL.ByteString
  -> Word32
  -> IO Word64
decodePersistedOffset p bs off =
  case DBG.runGetOrFail DBG.getWord64le (BSL.drop (fromIntegral off) bs) of
    Left (_, _, msg) -> X.throwIO (ME.ErrorReadingPersistenceFile p off msg)
    Right (_, _, w) -> return w

extract :: O.EOptions -> IO ()
extract eopts = do
  let persistFile = O.ePersistenceFile eopts
  persistedBytes <- BSL.readFile persistFile
  offsetMap <- readMappingFile (O.eVarMappingFile eopts)
  valueMap <- T.traverse (decodePersistedOffset persistFile persistedBytes) offsetMap
  let output = DA.encode valueMap
  case O.eExtractOutput eopts of
    Nothing -> BSC.hPutStrLn SI.stdout output
    Just outPath -> BSL.writeFile outPath output

handleMCTraceErrors :: ME.TraceException -> IO ()
handleMCTraceErrors te = do
  let layout = PP.layoutPretty PP.defaultLayoutOptions
  PPT.renderIO SI.stderr (layout (PP.pretty te <> PP.hardline))
  SE.exitFailure

main :: IO ()
main = do
  opts <- OA.execParser O.options
  case opts of
    O.Instrument iopts -> instrument iopts `X.catches` [X.Handler handleMCTraceErrors]
    O.Extract eopts -> extract eopts `X.catches` [X.Handler handleMCTraceErrors]
