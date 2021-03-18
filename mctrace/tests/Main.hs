{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import           Control.Monad ( when )
import qualified Control.Monad.Trans as CMT
import qualified Data.Binary.Get as DBG
import qualified Data.BitVector.Sized as DBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import qualified Data.List.NonEmpty as DLN
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text.IO as TIO
import           Data.Word ( Word32 )
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR
import qualified LLVM.AST as IR
import qualified LLVM.Analysis as LLA
import qualified LLVM.CodeGenOpt as LLCGO
import qualified LLVM.CodeModel as LLC
import qualified LLVM.Context as LLCX
import qualified LLVM.Module as LLM
import qualified LLVM.Relocation as LLR
import qualified LLVM.Target as LLT
import qualified System.Directory as SD
import qualified System.Exit as SE
import qualified System.FilePath as SF
import qualified System.FilePath.Glob as SFG
import qualified System.IO as SI
import qualified System.IO.Temp as SIT
import qualified System.Process as SP
import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as TTH
import qualified Test.Tasty.Hedgehog as TTHe
import qualified Test.Tasty.Options as TTO
import qualified Test.Tasty.Runners as TTR

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT
import qualified Language.DTrace.Syntax.Typed.Eval as LDTE
import qualified MCTrace.Codegen.LLVM as MCL

data SaveTemps = SaveTemps Bool
  deriving (Eq, Show)

instance TTO.IsOption SaveTemps where
  defaultValue = SaveTemps False
  parseValue v = SaveTemps <$> TTO.safeReadBool v
  optionName = pure "save-temps"
  optionHelp = pure "Save generated LLVM and object files to the system temporary directory"

matchingProbeDescription :: DLN.NonEmpty LD.ProbeDescription
                         -> LDT.Probe globals
                         -> Bool
matchingProbeDescription targetDescs (LDT.Probe theseDescs _ _ _) =
  targetDescs == theseDescs

-- | Due to the way sequences of probes are generated (i.e., based on index in
-- the list of probes, rather than from a list of concrete probe names), the
-- matching on this probe list is different than the others.
--
-- It matches a probe if the entire list of probe descriptions is exactly equal
-- to that of one of the probes (rather than attempting to reason about matching
-- wildcards)
evaluateProbes :: [LDT.Probe globals]
               -> LDTE.EvaluatorState globals
               -> DLN.NonEmpty LD.ProbeDescription
               -> H.PropertyT IO (LDTE.EvaluatorState globals)
evaluateProbes probes s0 desc
  | Just p <- F.find (matchingProbeDescription desc) probes = return (LDTE.evalProbe p s0)
  | otherwise = do
      H.footnote ("No probe for description: " ++ show desc)
      H.failure

-- | Compile the probe runner using gcc
compileRunner :: FilePath -> IO ()
compileRunner tmpDir = do
  ph <- SP.spawnProcess "gcc" ["tests/probe-runner.c", "-ldl", "-o", tmpDir SF.</> "probe-runner"]
  ec <- SP.waitForProcess ph
  case ec of
    SE.ExitSuccess -> return ()
    SE.ExitFailure c -> SE.die ("Error building probe runner: " ++ show c)

-- | Compile the generated probe object file (at @objPath@ on disk) into a
-- shared library that can be loaded into the probe runner.  This just uses gcc.
compileSharedLibrary :: FilePath -> FilePath -> IO FilePath
compileSharedLibrary tmpDir objPath = do
  let soPath = tmpDir SF.</> "probes.so"
  ph <- SP.spawnProcess "gcc" [objPath, "-shared", "-o", soPath]
  ec <- SP.waitForProcess ph
  case ec of
    SE.ExitSuccess -> return soPath
    SE.ExitFailure c -> TTH.assertFailure ("Failure compiling probes object to shared library: " ++ show c)

-- | Compute the size of the global storage area in the runner
--
-- NOTE: This currently assumes that all slots are size 8; this could be made
-- more precise by incorporating the variable definitions
globalsSize :: Ctx.Assignment LDT.GlobalVariable globals
            -> Ctx.Assignment (C.Const Word32) globals
            -> Int
globalsSize _varDefs varOffsets = 8 * Ctx.sizeInt (Ctx.size varOffsets)

readOffset :: DBG.Get a -> Word32 -> BS.ByteString -> a
readOffset g off bytes =
  DBG.runGet g (BSL.fromStrict (BS.drop (fromIntegral off) bytes))

-- | Read a bitvector of the given width at an offset into the given bytestring
readBV :: PN.NatRepr n -> Word32 -> BS.ByteString -> H.PropertyT IO (DBS.BV n)
readBV width offset bytes =
  case PN.natValue width of
    8 -> return (DBS.mkBV width (toInteger (readOffset DBG.getWord8 offset bytes)))
    16 -> return (DBS.mkBV width (toInteger (readOffset DBG.getWord16le offset bytes)))
    32 -> return (DBS.mkBV width (toInteger (readOffset DBG.getWord32le offset bytes)))
    64 -> return (DBS.mkBV width (toInteger (readOffset DBG.getWord64le offset bytes)))
    w -> do
      H.footnote ("Invalid bitvector width: " ++ show w)
      H.failure

-- | Check that the actual values of every global match the values computed by
-- the test runner (with the results stored in the bytestring)
assertEqualStates :: Ctx.Assignment LDT.GlobalVariable globals
                  -> Ctx.Assignment (C.Const Word32) globals
                  -> Ctx.Assignment LDTE.RegEntry globals
                  -> BS.ByteString
                  -> H.PropertyT IO ()
assertEqualStates globalVars globalOffsets oracleResults bytes =
  Ctx.forIndexM (Ctx.size globalVars) $ \idx -> do
    case LDT.globalVarRepr (globalVars Ctx.! idx) of
      LDT.BVRepr nr -> do
        actualBV <- readBV nr (C.getConst (globalOffsets Ctx.! idx)) bytes
        case oracleResults Ctx.! idx of
          LDTE.RegBV oracleBV -> do
            H.annotate ("Checking var " ++ show (globalVars Ctx.! idx))
            H.annotate ("  oracle = " ++ show oracleBV)
            H.annotate ("  actual = " ++ show actualBV)
            H.assert (oracleBV == actualBV)

-- | Parse and typecheck a DTrace probe script
loadProbes :: FilePath -> H.PropertyT IO LDT.Probes
loadProbes dfile = do
  dcontent <- CMT.lift $ TIO.readFile dfile
  case LD.parseDTrace dfile dcontent of
    Left err -> do
      H.footnote ("Error parsing DTrace file: " ++ show err)
      H.failure
    Right decls ->
      case LD.typeCheck decls of
        Left err -> do
          H.footnote ("Type errors: " ++ show err)
          H.failure
        Right probes -> return probes

-- | Use llvm-hs to compile the generated LLVM IR into an object file (saved to
-- @objPath@)
--
-- If the test harness has been directed to save temporary files, this will also
-- save the LLVM assembly and object file outside of the temporary test
-- directory (at the top level of the system temporary directory)
writeModuleToObject :: SaveTemps -> IR.Module -> FilePath -> IO ()
writeModuleToObject (SaveTemps saveTemps) llvmMod objPath =
  LLCX.withContext $ \ctx -> LLM.withModuleFromAST ctx llvmMod $ \nativeModule -> do
    LLA.verify nativeModule
    LLT.withHostTargetMachine LLR.Default LLC.Default LLCGO.Aggressive $ \tm -> do
      bs <- LLM.moduleObject tm nativeModule
      BS.writeFile objPath bs

      -- Save temporary objects and assembly if requested
      when saveTemps $ do
        let base = SF.takeFileName objPath
        tmpDir <- SD.getTemporaryDirectory
        LLM.writeLLVMAssemblyToFile (LLM.File (tmpDir SF.</> base SF.<.> "ll")) nativeModule
        BS.writeFile (tmpDir SF.</> base SF.<.> "o") bs

-- | For a given DTrace script, compile it to native code using LLVM and then
-- test the compiled code against the results from the Haskell evaluator
--
-- Random sequences of probe invocations are generated using Hedgehog so that
-- the test cases don't need to specify sequences manually.
--
-- The probe runner takes the path to the shared library and amount of storage
-- required, then reads a sequence of probe names from stdin (running the
-- prescribed probes).  It prints the final state of memory out on stdout before
-- exiting.  The harness collects that final state for comparison against the
-- oracle interpreter.
mkEvalTest :: FilePath -> FilePath -> TT.TestTree
mkEvalTest tmpDir dfile = TT.askOption $ \saveTemps@(SaveTemps _) -> TTHe.testProperty dfile $ H.property $ do
  probes <- loadProbes dfile
  case MCL.compileProbesLLVM (SF.takeBaseName dfile) probes of
    MCL.CompiledProbes varDefs varOffsets probeNames llvmMod -> do
      probeIndexes <- H.forAll (HG.list (HR.linear 0 20) (HG.element [0.. length probeNames - 1]))
      let probeSequence = [ probeNames !! idx | idx <- probeIndexes ]
      let objPath = tmpDir SF.</> "probes.o"
      CMT.lift $ writeModuleToObject saveTemps llvmMod objPath
      soPath <- CMT.lift $ compileSharedLibrary tmpDir objPath
      CMT.lift $ SD.removeFile objPath
      let runnerPath = tmpDir SF.</> "probe-runner"
      let nBytes = globalsSize varDefs varOffsets
      let runnerProc = SP.proc runnerPath [soPath, show nBytes]
      let p = runnerProc { SP.std_out = SP.CreatePipe
                         , SP.std_in = SP.CreatePipe
                         }
      (Just hdlIn, Just hdlOut, _, ph) <- CMT.lift $ SP.createProcess p
      F.forM_ probeSequence $ \(_, probeName) -> do
        H.annotate ("Calling probe: " ++ probeName)
        CMT.lift $ SI.hPutStrLn hdlIn probeName
      CMT.lift $ SI.hClose hdlIn

      -- Get the final global store of the test case, which the runner writes to stdout
      globalStore <- CMT.lift $ BS.hGet hdlOut nBytes

      -- Evaluate the test case against our Haskell oracle
      let s0 = LDTE.initialEvaluatorState LDT.globalVarRepr varDefs
      let seqDescs = fmap (probeDescription . fst) probeSequence
      LDTE.EvaluatorState s1 <- F.foldlM (evaluateProbes (fmap fst probeNames)) s0 seqDescs

      -- Verify that the machine code and the oracle agree
      assertEqualStates varDefs varOffsets s1 globalStore

      -- Cleanly shut down the process
      ec <- CMT.lift $ SP.waitForProcess ph
      case ec of
        SE.ExitSuccess -> return ()
        SE.ExitFailure c -> do
          H.footnote ("Probe runner failed: " ++ show c)
          H.failure

probeDescription :: LDT.Probe globals -> DLN.NonEmpty LD.ProbeDescription
probeDescription (LDT.Probe desc _ _ _) = desc

ingredients :: [TTR.Ingredient]
ingredients = TT.includingOptions [ TTO.Option (Proxy @SaveTemps)
                                  ] : TT.defaultIngredients

main :: IO ()
main = do
  LLT.initializeAllTargets
  SIT.withSystemTempDirectory "mctrace-tests" $ \tmpDir -> do
    compileRunner tmpDir
    evalExpectedPaths <- SFG.namesMatching "tests/eval/*.d"

    let group = TT.testGroup "MCTrace" [ TT.testGroup "EvalTests" (fmap (mkEvalTest tmpDir) evalExpectedPaths)
                                       ]
    TT.defaultMainWithIngredients ingredients group
