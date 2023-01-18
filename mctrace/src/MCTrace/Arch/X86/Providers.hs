{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
-- | Definitions of probe providers for x86_64
--
-- Note that the entry probes could fire *before* the syscall or *during* the
-- syscall (i.e., in user code or in the libc wrapper), depending on the binary.
--
-- We choose the strategy of firing probes *before* the call, as the binary
-- rewriter has trouble rewriting the syscall wrapper functions in glibc due to
-- misclassification of tail calls.  The disadvantage of this approach is that
-- it is possible that some call sites could be missed due to incompleteness of
-- the binary analysis.
--
-- This approach also better supports dynamically-linked binaries.
--
-- As a disadvantage, it does mean that indirect calls to functions will likely
-- not be instrumented.
module MCTrace.Arch.X86.Providers (
  matchProbes
  ) where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( guard )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as Map
import           Data.Maybe ( mapMaybe )
import qualified Data.Text as T
import qualified Debug.Trace as Trace

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
  -> [R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64)]
callProbe locationAnalysis repr@RX.X86Repr probeAddr =
  generatePushes ++
  [ RX.annotateInstrWith addMemAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RDI
                              , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
  , RX.annotateInstrWith addProbeSupportFnsAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RSI
                              , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]                              
  , RX.annotateInstrWith addJumpTarget $
      RX.makeInstr repr "call" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]
  ]
  ++ generatePops
  where
    argRegs = [F86.RDI, F86.RSI, F86.RDX, F86.RCX, F86.R8, F86.R9]
    generatePushes = map (\r -> RX.noAddr $ RX.makeInstr repr "push" [F86.QWordReg r]) argRegs
    generatePops = map (\r -> RX.noAddr $ RX.makeInstr repr "pop" [F86.QWordReg r]) (reverse argRegs)
    addJumpTarget (RX.AnnotatedOperand v _) =
      case v of
        (F86.JumpOffset {}, _) -> RX.AnnotatedOperand v (R.SymbolicRelocation probeAddr)
        _ -> RX.AnnotatedOperand v R.NoRelocation
    addMemAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.VoidMem {}, _) -> RX.AnnotatedOperand v (R.PCRelativeRelocation globalStorePtr)
        _ -> RX.AnnotatedOperand v R.NoRelocation
    addProbeSupportFnsAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.VoidMem {}, _) -> RX.AnnotatedOperand v (R.PCRelativeRelocation probeSupportFnsPtr)
        _ -> RX.AnnotatedOperand v R.NoRelocation
    globalStorePtr = MA.injectedStorePointer (MA.injectedAssets locationAnalysis)
    probeSupportFnsPtr = MA.probeSupportFunctionsPtr (MA.injectedAssets locationAnalysis)

-- | If the last instruction is a jump, return its symbolic target
--
-- On x86, it happens to be the case that all jump/call instructions just take a
-- single operand, which will have an associated relocation at this stage.
withLastInstructionSymTarget
  :: a
  -> R.SymbolicBlock RX.X86_64
  -> (R.SymbolicAddress RX.X86_64 -> a)
  -> a
withLastInstructionSymTarget def symBlock k =
  R.withSymbolicInstructions symBlock $ \_repr insns -> do
    case RX.toFlexInstF (DLN.head (DLN.reverse insns)) of
      Just ii
        | [RX.AnnotatedOperand (F86.JumpOffset {}, _) (R.SymbolicRelocation symAddr)] <- F86.iiArgs ii ->
          k symAddr
      _ -> def

symAddrForSymbol
  :: MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> String
  -> Maybe (R.SymbolicAddress RX.X86_64)
symAddrForSymbol locationAnalysis symName = do
  entryAddr <- Map.lookup (BSC.pack symName) (MA.functionEntryPoints locationAnalysis)
  Map.lookup entryAddr (MA.symbolicAddresses locationAnalysis) <|> Just (R.stableAddress entryAddr)

matcherEntry
  :: LD.ProbeDescription
  -> [String]
  -> MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> R.SymbolicBlock RX.X86_64
  -> Maybe (MP.ProbeInserter RX.X86_64)
matcherEntry providerName symNames locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  -- The mapping from symbolic addresses to concrete addresses can be found
  -- in the configuration (via 'R.getSymbolicBlockMap')
  let symAddrs = mapMaybe (symAddrForSymbol locationAnalysis) symNames
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (any (== lastSymTgt) symAddrs)
    let assets = MA.injectedAssets locationAnalysis
    let probeSymAddrs = mapMaybe (matchProvider providerName) (MA.injectedProbeAddrs assets)
    return $ MP.ProbeInserter $ \irep insns ->
      let term DLN.:| rest = DLN.reverse insns
          callSequence = concatMap (callProbe locationAnalysis irep) probeSymAddrs
          rinsns = term DLN.:| (reverse callSequence <> rest)
      in DLN.reverse rinsns

matcherExit
  :: LD.ProbeDescription
  -> [String]
  -> MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> R.SymbolicBlock RX.X86_64
  -> Maybe (MP.ProbeInserter RX.X86_64)
matcherExit providerName symNames locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  -- The mapping from symbolic addresses to concrete addresses can be found
  -- in the configuration (via 'R.getSymbolicBlockMap')
  let symAddrs = mapMaybe (symAddrForSymbol locationAnalysis) symNames
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (any (== lastSymTgt) symAddrs)
    let assets = MA.injectedAssets locationAnalysis
    let probeSymAddrs = mapMaybe (matchProvider providerName) (MA.injectedProbeAddrs assets)
    return $ MP.ProbeInserter $ \irep insns ->
      -- FIXME: Currently the instructions to insert the probe are the same, irrespective
      -- of whether it is at entry or exit. This is likely to change in the future. So, it
      -- may be good to have a separate version of `callProbe` for exit probes.
      let callSequence = concatMap (callProbe locationAnalysis irep) probeSymAddrs
          term DLN.:| rest = insns
      in Trace.traceShow providerName $ term DLN.:| (rest <> callSequence)

matchProbes :: MA.ProbeLocationAnalysisResult globals RX.X86_64
            -> R.SymbolicBlock RX.X86_64
            -> [MP.ProbeInserter RX.X86_64]
matchProbes probeLocations symBlock = do
  let probeInserterPairs = mapMaybe (\p -> (p, ) <$> MP.providerMatcher p probeLocations symBlock) providers
  map snd (reorder probeInserterPairs)
  where
    -- FIXME: The precise ordering needs more thought and especially how it interacts
    -- with the insertion process in `rewrite`
    -- Also using names like "entry" and "return" in this function is likely
    -- error prone. Consider another classification method
    reorder probeInserterPairs = 
      let entryProbes = filter (\(p, _) -> probeName p == T.pack "entry") probeInserterPairs
          exitProbes = filter (\(p, _) -> probeName p == T.pack "return") probeInserterPairs
      in entryProbes ++ exitProbes
    probeName p =  LD.probeName (MP.providerName p)

-- | A probe that fires at the entry to `read` (the user-level wrapper around
-- the syscall, provided by libc)
--
-- Precisely speaking, this provider fires when the last instruction of the
-- block is statically a call to the syscall wrapper for read.
--
-- Note that this currently only works for statically linked binaries, as it
-- inserts the probe at the entry to the syscall wrapper (which isn't accessible
-- in a dynamically-linked binary)
readEntrySyscallProvider :: MP.ProbeProvider globals RX.X86_64
readEntrySyscallProvider =
  MP.ProbeProvider { MP.providerName = name
                   , MP.providerDescription = desc
                   , MP.providerMatcher = matcherEntry name ["read", "read@plt"]
                   }
  where
    name = LD.ProbeDescription { LD.probeProvider = T.pack "mctrace"
                               , LD.probeModule = T.pack "syscall"
                               , LD.probeFunction = T.pack "read"
                               , LD.probeName = T.pack "entry"
                               }
    desc = PP.hsep [ PP.pretty "Probe fires at the entry to `read` system calls"
                   ]

writeEntrySyscallProvider :: MP.ProbeProvider globals RX.X86_64
writeEntrySyscallProvider =
  MP.ProbeProvider { MP.providerName = name
                   , MP.providerDescription = desc
                   , MP.providerMatcher = matcherEntry name ["write", "write@plt"]
                   }
  where
    name = LD.ProbeDescription { LD.probeProvider = T.pack "mctrace"
                               , LD.probeModule = T.pack "syscall"
                               , LD.probeFunction = T.pack "write"
                               , LD.probeName = T.pack "entry"
                               }
    desc = PP.hsep [ PP.pretty "Probe fires at the entry to `write` system calls"
                   ]

openEntrySyscallProvider :: MP.ProbeProvider globals RX.X86_64
openEntrySyscallProvider =
  MP.ProbeProvider { MP.providerName = name
                   , MP.providerDescription = desc
                   , MP.providerMatcher = matcherEntry name ["open", "open@plt"]
                   }
  where
    name = LD.ProbeDescription { LD.probeProvider = T.pack "mctrace"
                               , LD.probeModule = T.pack "syscall"
                               , LD.probeFunction = T.pack "open"
                               , LD.probeName = T.pack "entry"
                               }
    desc = PP.hsep [ PP.pretty "Probe fires at the entry to `open` system calls"
                   ]

readReturnSyscallProvider :: MP.ProbeProvider globals RX.X86_64
readReturnSyscallProvider =
  MP.ProbeProvider { MP.providerName = name
                   , MP.providerDescription = desc
                   , MP.providerMatcher = matcherExit name ["read", "read@plt"]
                   }
  where
    name = LD.ProbeDescription { LD.probeProvider = T.pack "mctrace"
                               , LD.probeModule = T.pack "syscall"
                               , LD.probeFunction = T.pack "read"
                               , LD.probeName = T.pack "return"
                               }
    desc = PP.hsep [ PP.pretty "Probe fires at the return from `read` system calls"
                   ]

writeReturnSyscallProvider :: MP.ProbeProvider globals RX.X86_64
writeReturnSyscallProvider =
  MP.ProbeProvider { MP.providerName = name
                   , MP.providerDescription = desc
                   , MP.providerMatcher = matcherExit name ["write", "write@plt"]
                   }
  where
    name = LD.ProbeDescription { LD.probeProvider = T.pack "mctrace"
                               , LD.probeModule = T.pack "syscall"
                               , LD.probeFunction = T.pack "write"
                               , LD.probeName = T.pack "return"
                               }
    desc = PP.hsep [ PP.pretty "Probe fires at the return from `write` system calls"
                   ]

providers :: [MP.ProbeProvider globals RX.X86_64]
providers = [ readEntrySyscallProvider
            , readReturnSyscallProvider
            , writeEntrySyscallProvider
            , writeReturnSyscallProvider
            , openEntrySyscallProvider
            ]
