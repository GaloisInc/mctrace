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
import qualified Data.Macaw.Memory as MM
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
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env)
  => MC.ProbeIndex globals w
  -> env RX.X86_64 binFmt
  -> R.RewriteM MA.LogEvent RX.X86_64 (MA.InjectedAssets globals RX.X86_64)
preAnalyze probeIndex _env = do
  probeAddrs <- DT.forM (MC.probeOffsets probeIndex) $ \(p, probeSymbol, bytes) -> do
    symPtr <- R.injectFunction ("__mctrace_" ++ probeSymbol) bytes
    return (p, symPtr)
  storePtrAddr <- R.newGlobalVar "__mctrace_probeStore" (fromIntegral (PN.natValue (MC.pointerWidth probeIndex)) `div` 8)
  return MA.InjectedAssets { MA.injectedProbeAddrs = probeAddrs
                           , MA.injectedStorePointer = storePtrAddr
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

-- | Return true if this 'R.SymbolicBlock' is the program entry point
--
-- Note that the 'MBL.entryPoints' function always returns the named program
-- entry point as the first element
isEntryPoint
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env, MBL.BinaryLoader RX.X86_64 binFmt)
  => env RX.X86_64 binFmt
  -> R.SymbolicBlock RX.X86_64
  -> Bool
isEntryPoint env symBlock =
  Just (R.absoluteAddress (R.symbolicBlockOriginalAddress symBlock)) == MM.segoffAsAbsoluteAddr programEntry
  where
    binary = R.analysisLoadedBinary env
    Just (programEntry DLN.:| _) = MBL.entryPoints binary

-- | Prepend a list to a non-empty list, producing another non-empty list (even if the left-hand list is empty)
prepend :: [a] -> DLN.NonEmpty a -> DLN.NonEmpty a
prepend l nel =
  case l of
    [] -> nel
    (i1:rest) -> (i1 DLN.:| rest) <> nel

-- | Rewrite the binary at the basic block level
--
-- The rewriter is highly local, but has access to the analysis and pre-rewriter
-- states.
rewrite
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env, MBL.BinaryLoader RX.X86_64 binFmt)
  => MC.ProbeIndex globals w
  -> env RX.X86_64 binFmt
  -> MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> C.Const () RX.X86_64
  -> R.SymbolicBlock RX.X86_64
  -> R.RewriteM MA.LogEvent RX.X86_64 (Maybe (R.ModifiedInstructions RX.X86_64))
rewrite probeIndex env probeLocations _ symBlock =
  R.withSymbolicInstructions symBlock $ \irepr insns -> do
    let storageFile = MC.probeStorageFile probeIndex
    let storageBytes = MC.probeStorageBytes probeIndex
    let globalAddr = MA.injectedStorePointer (MA.injectedAssets probeLocations)
    let initCode = MAS.linuxInitializationCode storageFile storageBytes globalAddr irepr
    let probeInserters = mapMaybe (\p -> MP.providerMatcher p probeLocations symBlock) MAP.providers
    case probeInserters of
      []
        | isEntryPoint env symBlock -> do
            return (Just (R.ModifiedInstructions irepr (initCode `prepend` insns)))
        | otherwise -> return Nothing
      _ -> do
          let insns' = foldr (\p is -> MP.insertProbe p irepr is) insns probeInserters
          if | isEntryPoint env symBlock ->
               return (Just (R.ModifiedInstructions irepr (initCode `prepend` insns')))
             | otherwise -> return (Just (R.ModifiedInstructions irepr insns'))

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
