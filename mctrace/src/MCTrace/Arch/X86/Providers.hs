{-# LANGUAGE GADTs #-}
-- | Definitions of probe providers for x86_64
--
-- Note that the entry probes could fire *before* the syscall or *during* the
-- syscall (i.e., in user code or in the libc wrapper), depending on the binary.
-- For static binaries, we can fire the probe in the libc wrapper.  For
-- dynamically linked binaries, we would need to fire them before (from user
-- code), because the syscall stub definitions are not available.
--
-- Note further that, for binaries without a libc or equivalent, we would need
-- to have more specific hooks.
module MCTrace.Arch.X86.Providers (
  providers
  ) where

import           Control.Monad ( guard )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as Map
import           Data.Maybe ( mapMaybe )
import qualified Data.Text as T
import qualified Flexdis86 as F86
import qualified Prettyprinter as PP
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as RX

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT
import qualified MCTrace.Analysis as MA
import qualified MCTrace.ProbeProvider as MP

-- | Attempt to match a user provided probe against a probe provider; if the
-- provider satisfies the user probe, return the address of the probe that
-- should be invoked.
matchProvider
  :: LD.ProbeDescription
  -- ^ The full description of the probe provider
  -> (LDT.Probe globals, R.SymbolicAddress arch)
  -- ^ The user probe to match against
  -> Maybe (R.SymbolicAddress arch)
matchProvider name (LDT.Probe descs _ _ _, addr) = do
  guard (F.any (LD.probeDescriptionMatches name) descs)
  return addr

-- | Prepend a list to a non-empty list, producing another non-empty list (even if the left-hand list is empty)
prepend :: [a] -> DLN.NonEmpty a -> DLN.NonEmpty a
prepend l nel =
  case l of
    [] -> nel
    (i1:rest) -> (i1 DLN.:| rest) <> nel

-- | Make an instruction sequence to call a probe
--
-- This has to restore the environment to exactly what it was before the call.
-- It needs to pass the pointer to global storage as the only argument (for now;
-- syscall parameters can be made available to probes later).  The first
-- parameter to a function (under the sysv ABI) is passed in %rdi.
--
-- > push %rdi
-- > mov 0x0(globalvar), %rdi
-- > call probe
-- > pop %rdi
--
-- Note that we generate a placeholder offset for the @mov@ instruction; we
-- don't know the real offset to plug in until this code is actually inserted
-- and has its relocations fixed up.  We represent the real location by
-- annotating that operand with the concrete address allocated to the variable
-- we load.
callProbe
  :: MA.ProbeLocationAnalysisResult globals RX.X86_64
  -- ^ Analysis results we can draw from (including the pointer to global storage)
  -> R.InstructionArchRepr RX.X86_64 tp
  -- ^ The instruction repr (only one for x86_64)
  -> R.SymbolicAddress RX.X86_64
  -- ^ The symbolic address of the injected probe function
  -> [R.TaggedInstruction RX.X86_64 tp (R.InstructionAnnotation RX.X86_64)]
callProbe locationAnalysis repr@RX.X86Repr probeAddr =
  [ R.tagInstruction Nothing $ RX.noAddr $ RX.makeInstr repr "push" [F86.QWordReg F86.RDI]
  , R.tagInstruction Nothing $
      RX.annotateInstrWith addMemAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RDI
                              , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
  , R.tagInstruction (Just probeAddr) $ RX.noAddr $ RX.makeInstr repr "call" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]
  , R.tagInstruction Nothing $ RX.noAddr $ RX.makeInstr repr "pop" [F86.QWordReg F86.RDI]
  ]
  where
    addMemAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.VoidMem {}, _) -> RX.AnnotatedOperand v (RX.AbsoluteAddress globalStorePtr)
        _ -> RX.AnnotatedOperand v RX.NoAddress
    globalStorePtr = MA.injectedStorePointer (MA.injectedAssets locationAnalysis)

-- | A probe that fires at the entry to `read` (the user-level wrapper around
-- the syscall, provided by libc)
--
-- Note that this currently only works for statically linked binaries, as it
-- inserts the probe at the entry to the syscall wrapper (which isn't accessible
-- in a dynamically-linked binary)
readEntrySyscallProvider :: MP.ProbeProvider globals RX.X86_64
readEntrySyscallProvider =
  MP.ProbeProvider { MP.providerName = name
                   , MP.providerDescription = desc
                   , MP.providerMatcher = matcher
                   }
  where
    name = LD.ProbeDescription { LD.probeProvider = T.pack "mctrace"
                               , LD.probeModule = T.pack "syscall"
                               , LD.probeFunction = T.pack "read"
                               , LD.probeName = T.pack "entry"
                               }
    desc = PP.hsep [ PP.pretty "Probe fires at the entry to `read` system calls"
                   ]
    matcher locationAnalysis symBlock = do
      entryAddr <- Map.lookup (BSC.pack "read") (MA.functionEntryPoints locationAnalysis)
      guard (R.symbolicBlockOriginalAddress symBlock == entryAddr)
      let assets = MA.injectedAssets locationAnalysis
      let probeSymAddrs = mapMaybe (matchProvider name) (MA.injectedProbeAddrs assets)
      return $ MP.ProbeInserter $ \irep insns ->
        let callSequences = concatMap (callProbe locationAnalysis irep) probeSymAddrs
        in prepend callSequences insns

_readReturnSyscallProvider :: MP.ProbeProvider globals arch
_readReturnSyscallProvider =
  MP.ProbeProvider { MP.providerName = name
                   , MP.providerDescription = desc
                   }
  where
    name = LD.ProbeDescription { LD.probeProvider = T.pack "mctrace"
                               , LD.probeModule = T.pack "syscall"
                               , LD.probeFunction = T.pack "read"
                               , LD.probeName = T.pack "return"
                               }
    desc = PP.hsep [ PP.pretty "Probe fires at the return from `read` system calls"
                   ]

providers :: [MP.ProbeProvider globals RX.X86_64]
providers = [ readEntrySyscallProvider
            ]
