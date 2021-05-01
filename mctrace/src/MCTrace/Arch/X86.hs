{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
module MCTrace.Arch.X86 (
  x86Rewriter
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as EE
import qualified Data.ElfEdit.Prim as EEP
import qualified Data.Foldable as F
import qualified Data.Functor.Const as C
import qualified Data.List.NonEmpty as DLN
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Macaw.Discovery.State as DMD
import           Data.Macaw.X86.Symbolic ()
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe, listToMaybe, mapMaybe )
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
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env, MBL.BinaryLoader RX.X86_64 binFmt, binFmt ~ EE.ElfHeaderInfo 64)
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
  :: (R.ArchConstraints arch, R.HasAnalysisEnv env, R.HasSymbolicBlockMap env, binFmt ~ EE.ElfHeaderInfo 64)
  => env arch binFmt
  -> Map.Map BS.ByteString (R.ConcreteAddress arch)
indexFunctionEntries env =
  pltStubSymbols env <> Map.fromList normalSymbols
  where
    blockInfo = R.analysisBlockInfo env
    normalSymbols = [ (fromMaybe (BSC.pack (show entryAddr)) (DMD.discoveredFunSymbol dfi), entryAddr)
                    | (entryAddr, (_blockAddrs, Some dfi)) <- Map.toList (R.biFunctions blockInfo)
                    ]

-- | Match up names PLT stub entries
--
-- Calls to functions in shared libraries are issued through PLT stubs. These
-- are short sequences included in the binary by the compiler that jump to the
-- *real* function implementation in the shared library via the Global Offset
-- Table.  The GOT is populated by the dynamic loader.
--
-- The name for the PLT stub for the @read@ function will be named @read\@plt@.
--
-- See Note [PLT Stub Names] for details
pltStubSymbols
  :: (R.ArchConstraints arch, R.HasAnalysisEnv env, R.HasSymbolicBlockMap env, binFmt ~ EE.ElfHeaderInfo 64)
  => env arch binFmt
  -> Map.Map BS.ByteString (R.ConcreteAddress arch)
pltStubSymbols env = Map.fromList $ fromMaybe [] $ do
  vam <- EEP.virtAddrMap elfBytes phdrs
  eres <- EEP.getDynamicSectionFromSegments elfHeader phdrs elfBytes vam
  dynSec <- case eres of
    Left _dynErr -> Nothing
    Right dynSec -> return dynSec
  relas <- case EEP.dynPLTRel @EEP.X86_64_RelocationType dynSec of
    Right (EEP.PLTRela relas) -> return relas
    _ -> Nothing
  let (_, revNameRelaMap) = F.foldl' (pltStubAddress dynSec) (1, []) relas
  let nameRelaMap = zip [0..] (reverse revNameRelaMap)
  let (_, elf) = EE.getElf elfHeaderInfo
  pltGotSec <- listToMaybe (EE.findSectionByName (BSC.pack ".plt.got") elf)
  let pltGotBase = EE.elfSectionAddr pltGotSec
  return [ (symName <> BSC.pack "@plt", R.concreteFromAbsolute (fromIntegral ((idx + 1) * 16 + pltGotBase)))
         | (idx, (symName, _)) <- nameRelaMap
         ]
  where
    bin = R.analysisLoadedBinary env
    elfHeaderInfo = MBL.originalBinary bin
    elfHeader = EE.header elfHeaderInfo
    phdrs = EE.headerPhdrs elfHeaderInfo
    elfBytes = EE.headerFileContents elfHeaderInfo

    pltStubAddress dynSec (idx, accum) rela
      | Right (symtabEntry, _versionedVal) <- EEP.dynSymEntry dynSec idx
      , EE.steType symtabEntry == EE.STT_FUNC =
        (idx + 1, (EE.steName symtabEntry, EE.relaAddr rela) : accum)
      | otherwise = pltStubAddress dynSec (idx+1, accum) rela

-- | Analyze the binary in the context of the pre-analysis state
analyze
  :: (R.HasAnalysisEnv env, R.HasSymbolicBlockMap env,  binFmt ~ EE.ElfHeaderInfo 64)
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
  :: (MBL.BinaryLoader RX.X86_64 binFmt, binFmt ~ EE.ElfHeaderInfo 64)
  => MC.ProbeIndex globals w
  -> R.AnalyzeAndRewrite MA.LogEvent RX.X86_64 binFmt (MA.ProbeLocationAnalysisResult globals)
x86Rewriter probes =
  R.AnalyzeAndRewrite { R.arPreAnalyze = preAnalyze probes
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
