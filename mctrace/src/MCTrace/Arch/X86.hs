{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
module MCTrace.Arch.X86 (
  x86Rewriter
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Functor.Const as C
import qualified Data.List.NonEmpty as DLN
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Macaw.Discovery.State as DMD
import           Data.Macaw.X86.Symbolic ()
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe, mapMaybe )
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Traversable as DT
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as RX

import qualified MCTrace.Analysis as MA
import qualified MCTrace.Arch.X86.Providers as MAP
import qualified MCTrace.Arch.X86.Setup as MAS
import qualified MCTrace.Codegen as MC
import qualified MCTrace.ProbeProvider as MP


-- | Run a pre-analysis pass in the rewriter monad ('R.RewriteM')
--
-- This pass has the ability to add code and global variables to the binary in a
-- way that allows us to record the new addresses.  These addresses can then
-- factor into the more general analysis.
preAnalyze
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env, MBL.BinaryLoader RX.X86_64 binFmt)
  => MC.ProbeIndex globals w
  -> env RX.X86_64 binFmt
  -> R.RewriteM MA.LogEvent RX.X86_64 (MA.InjectedAssets globals RX.X86_64)
preAnalyze probeIndex env = do
  probeAddrs <- DT.forM (MC.probeOffsets probeIndex) $ \(p, probeSymbol, bytes) -> do
    symPtr <- R.injectFunction ("__mctrace_" ++ probeSymbol) bytes
    return (p, symPtr)
  storePtrAddr <- R.newGlobalVar "__mctrace_probeStore" (fromIntegral (PN.natValue (MC.pointerWidth probeIndex)) `div` 8)

  let loadedBinary = R.analysisLoadedBinary env
  let mem = MBL.memoryImage loadedBinary
  let Just (origEntrySegoff DLN.:| _) = MBL.entryPoints loadedBinary
  let Just origEntryAddr = R.concreteFromSegmentOff mem origEntrySegoff

  let storageFile = MC.probeStorageFile probeIndex
  let storageBytes = MC.probeStorageBytes probeIndex
  let initCode = MAS.linuxInitializationCode storageFile storageBytes storePtrAddr RX.X86Repr origEntryAddr
  setupSymAddr <- R.injectInstructions "__mctrace_setup" RX.X86Repr initCode
  return MA.InjectedAssets { MA.injectedProbeAddrs = probeAddrs
                           , MA.injectedStorePointer = storePtrAddr
                           , MA.injectedEntryAddr = setupSymAddr
                           }

indexFunctionEntries
  :: (R.InstructionConstraints arch, R.HasAnalysisEnv env, R.HasSymbolicBlockMap env)
  => env arch binFmt
  -> Map.Map BS.ByteString (R.ConcreteAddress arch)
indexFunctionEntries env = Map.fromList
  [ (fromMaybe (BSC.pack (show entryAddr)) (DMD.discoveredFunSymbol dfi), entryAddr)
  | (entryAddr, (_blockAddrs, Some dfi)) <- Map.toList (R.biFunctions blockInfo)
  ]
  where
    blockInfo = R.analysisBlockInfo env

-- | Analyze the binary in the context of the pre-analysis state
analyze
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env)
  => env RX.X86_64 binFmt
  -> MA.InjectedAssets globals RX.X86_64
  -> IO (MA.ProbeLocationAnalysisResult globals RX.X86_64)
analyze env assets =
  return MA.ProbeLocationAnalysisResult { MA.injectedAssets = assets
                                        , MA.functionEntryPoints = indexFunctionEntries env
                                        , MA.symbolicAddresses = fmap R.symbolicBlockSymbolicAddress (R.getSymbolicBlockMap env)
                                        }

-- | Run another pass before rewriting that enables global modifications
--
-- This pass has access to the state produced by the analysis
preRewrite
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env)
  => env RX.X86_64 binFmt
  -> MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> R.RewriteM MA.LogEvent RX.X86_64 (C.Const () RX.X86_64)
preRewrite _ _ = return (C.Const ())

-- | Rewrite the binary at the basic block level
--
-- The rewriter is highly local, but has access to the analysis and pre-rewriter
-- states.
rewrite
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env)
  => MC.ProbeIndex globals w
  -> env RX.X86_64 binFmt
  -> MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> C.Const () RX.X86_64
  -> R.SymbolicBlock RX.X86_64
  -> R.RewriteM MA.LogEvent RX.X86_64 (Maybe (R.ModifiedInstructions RX.X86_64))
rewrite _probeIndex _env probeLocations _ symBlock =
  R.withSymbolicInstructions symBlock $ \irepr insns -> do
    let probeInserters = mapMaybe (\p -> MP.providerMatcher p probeLocations symBlock) MAP.providers
    case probeInserters of
      [] -> return Nothing
      _ -> do
          let insns' = foldr (\p is -> MP.insertProbe p irepr is) insns probeInserters
          return (Just (R.ModifiedInstructions irepr insns'))

x86Rewriter
  :: (MBL.BinaryLoader RX.X86_64 binFmt)
  => MC.ProbeIndex globals w
  -> R.AnalyzeAndRewrite MA.LogEvent RX.X86_64 binFmt (MA.ProbeLocationAnalysisResult globals)
x86Rewriter probes =
  R.AnalyzeAndRewrite { R.arPreAnalyze = preAnalyze probes
                      , R.arAnalyze = analyze
                      , R.arPreRewrite = preRewrite
                      , R.arRewrite = rewrite probes
                      }
