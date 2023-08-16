{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module MCTrace.Arch.PPC (
  ppcRewriter
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as EE
import qualified Data.Functor.Const as C
import qualified Data.List.NonEmpty as DLN
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.PPC ()
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery.State as DMD
import           Data.Macaw.PPC.Symbolic ()
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe )
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Traversable as DT
import qualified Renovate as R
import qualified Renovate.Arch.PPC as RX

import qualified MCTrace.Analysis as MA
import           MCTrace.Arch.Common ( injectPlatformApiImpl )
import qualified MCTrace.Arch.PPC.Providers as MAP
import qualified MCTrace.Arch.PPC.Setup as MAS
import qualified MCTrace.Codegen as MC
import qualified MCTrace.RuntimeAPI as RT
import qualified MCTrace.ProbeProvider as MP

-- | Run a pre-analysis pass in the rewriter monad ('R.RewriteM')
--
-- This pass has the ability to add code and global variables to the binary in a
-- way that allows us to record the new addresses.  These addresses can then
-- factor into the more general analysis.
preAnalyze
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env, MBL.BinaryLoader RX.PPC32 binFmt, binFmt ~ EE.ElfHeaderInfo 32)
  => MC.ProbeIndex globals w
  -> EE.ElfHeaderInfo n
  -> env RX.PPC32 binFmt
  -> R.RewriteM MA.LogEvent RX.PPC32 (MA.InjectedAssets globals RX.PPC32)
preAnalyze probeIndex library env = do
  probeAddrs <- DT.forM (MC.probeOffsets probeIndex) $ \(p, probeSymbol, bytes) -> do
    symPtr <- R.injectFunction ("__mctrace_" ++ probeSymbol) bytes
    return (p, symPtr)
  let pointerWidth = fromIntegral (PN.natValue (MC.pointerWidth probeIndex)) `div` 8
  storePtrAddr <- R.newGlobalVar "__mctrace_probeStore" pointerWidth

  supportFunAddrMap <- injectPlatformApiImpl library RT.supportFunctionNameMap

  let supportFunArraySize = pointerWidth * fromIntegral (length RT.probeSupportFunctions)
  probeSupportFunArrayAddr <- R.newGlobalVar "__mctrace_probeSupportFuns" supportFunArraySize

  let loadedBinary = R.analysisLoadedBinary env
  let mem = MBL.memoryImage loadedBinary
  let Just (origEntrySegoff DLN.:| _) = MBL.entryPoints loadedBinary
  let Just origEntryAddr = R.concreteFromSegmentOff mem origEntrySegoff

  let globalStorageSize = MC.probeGlobalStorageSize probeIndex
  let initCode = MAS.initializationCode globalStorageSize storePtrAddr supportFunAddrMap probeSupportFunArrayAddr RX.PPCRepr pointerWidth origEntryAddr
  setupSymAddr <- R.injectInstructions "__mctrace_setup" RX.PPCRepr initCode
  return MA.InjectedAssets { MA.injectedProbeAddrs = probeAddrs
                           , MA.injectedStorePointer = storePtrAddr
                           , MA.probeSupportFunctionsPtr = probeSupportFunArrayAddr
                           , MA.injectedEntryAddr = setupSymAddr
                           }

-- type instance PLT.ArchRelocationType RX.PPC32 = EEP.X86_64_RelocationType

indexFunctionEntries
  :: (R.ArchConstraints arch
     , R.HasAnalysisEnv env
     , R.HasSymbolicBlockMap env
     , binFmt ~ EE.ElfHeaderInfo (MC.ArchAddrWidth arch)
     , arch ~ RX.PPC32
     )
  => env arch binFmt
  -> Map.Map BS.ByteString (R.ConcreteAddress arch)
indexFunctionEntries env =
  {- PLT.pltStubSymbols env <> -} Map.fromList normalSymbols
  where
    blockInfo = R.analysisBlockInfo env
    normalSymbols = [ (fromMaybe (BSC.pack (show entryAddr)) (DMD.discoveredFunSymbol dfi), entryAddr)
                    | (entryAddr, (_blockAddrs, Some dfi)) <- Map.toList (R.biFunctions blockInfo)
                    ]

-- | Analyze the binary in the context of the pre-analysis state
analyze
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env,  binFmt ~ EE.ElfHeaderInfo 32)
  => env RX.PPC32 binFmt
  -> MA.InjectedAssets globals RX.PPC32
  -> IO (MA.ProbeLocationAnalysisResult globals RX.PPC32)
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
  => env RX.PPC32 binFmt
  -> MA.ProbeLocationAnalysisResult globals RX.PPC32
  -> R.RewriteM MA.LogEvent RX.PPC32 (C.Const () RX.PPC32)
preRewrite _ _ = return (C.Const ())

-- | Rewrite the binary at the basic block level
--
-- The rewriter is highly local, but has access to the analysis and pre-rewriter
-- states.
rewrite
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env)
  => MC.ProbeIndex globals w
  -> env RX.PPC32 binFmt
  -> MA.ProbeLocationAnalysisResult globals RX.PPC32
  -> C.Const () RX.PPC32
  -> R.SymbolicBlock RX.PPC32
  -> R.RewriteM MA.LogEvent RX.PPC32 (Maybe (R.ModifiedInstructions RX.PPC32))
rewrite _probeIndex _env probeLocations _ symBlock =
  R.withSymbolicInstructions symBlock $ \irepr insns -> do
    let probeInserters = MAP.matchProbes probeLocations symBlock
    case probeInserters of
      [] -> return Nothing
      _ -> do
          let insns' = foldl (\is p -> MP.insertProbe p irepr is) insns probeInserters
          return (Just (R.ModifiedInstructions irepr insns'))

ppcRewriter
  :: (MBL.BinaryLoader RX.PPC32 binFmt, binFmt ~ EE.ElfHeaderInfo 32)
  => MC.ProbeIndex globals w
  -> EE.ElfHeaderInfo n
  -> R.AnalyzeAndRewrite MA.LogEvent RX.PPC32 binFmt (MA.ProbeLocationAnalysisResult globals)
ppcRewriter probes library =
  R.AnalyzeAndRewrite { R.arPreAnalyze = preAnalyze probes library
                      , R.arAnalyze = analyze
                      , R.arPreRewrite = preRewrite
                      , R.arRewrite = rewrite probes
                      }

{- Note [PLT Stub Names]

In a dynamically linked binary, the compiler issues calls to shared library
functions by jumping to a PLT stub. The PLT stub jumps to an address taken from
the Global Offset Table (GOT), which is populated by the dynamic loader based on
where the shared library is mapped into memory.

These PLT stubs are not inherently assigned names, but we need names for
matching probe providers (i.e., if we want to instrument calls to @read@, we
need to be able to instrument @read\@plt@ in a dynamically linked binary).

PLT stubs do not have their own names in any symbol table. Instead, entries in
the Global Offset Table have names in the form of dynamic PLT relocations.  We
get those from elf-edit via the 'EEP.dynPLTRel' function. Note that these
relocations do not have their own directly associated names; instead, there is a
list of rela entries and a separate list of symbols. The list of rela entries
contains function relocations while the list of dynamic symbols
('EEP.dynSymEntry') contains both function and object symbols. To align them, we
must just discard the non-function entries. We do this by checking if the
current symbol entry is of function type; if it is not, we just grab the next
function symbol in the list.

That step gives us names for global offset table entries, but *not* names for
PLT stubs. We rely on the fact that the list of PLT stubs is in the same order
as the list of global offset table entries.  The previous step gives us the
*index* of each entry and a name for that entry. To get the name of the PLT stub
itself, we just compute the relevant offset from the base of the .plt.got
section.  Each PLT stub is 16 bytes on most architectures. The address of the
PLT stub of an entry is @addrOf(.plt.got) + 16 * (1 + idx)@. The offset of one
seems to be required because the first entry is some other kind of metadata or
otherwise ignored.

-}
