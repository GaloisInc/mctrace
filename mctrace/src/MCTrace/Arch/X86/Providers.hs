{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Monad ( guard )
import           Control.Exception ( assert )
import qualified Data.List.NonEmpty as DLN
import           Data.Maybe ( mapMaybe, catMaybes )

import qualified Flexdis86 as F86
import qualified Prettyprinter as PP
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as RX

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT
import qualified Language.DTrace.ProbeDescription as LDP
import qualified MCTrace.Analysis as MA
import qualified MCTrace.ProbeProvider as MP
import qualified MCTrace.Arch.Common as Common

-- | Make an instruction sequence to call an entry probe
--
-- This has to restore the environment to exactly what it was before the call.
-- It currently passes the following arguments to the probe (in order):
--  + Pointer to the global storage
--  + An array of support functions that the probe can access
--  + The `arg0` value - the first argument to function being probed
--  + The `ucaller` value - specified to be the address of the probe call instruction
--
-- Note that we rely on renovate's relocation support to post-facto fix up addresses
-- by annotating the corresponding arguments.
callEntryProbe
  :: MA.ProbeLocationAnalysisResult globals RX.X86_64
  -- ^ Analysis results we can draw from (including the pointer to global storage)
  -> R.InstructionArchRepr RX.X86_64 tp
  -- ^ The instruction repr (only one for x86_64)
  -> R.SymbolicAddress RX.X86_64
  -- ^ The symbolic address of the injected probe function
  -> [R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64)]
callEntryProbe locationAnalysis repr@RX.X86Repr probeAddr =
  generatePushes ++
  [ -- NOTE: The first argument to the function being probed (`arg0`) needs to be passed to the
    --       probe. The argument can be found in RDI at this point. While we need to pass this
    --       value as the 3rd argument to the probe (i.e. RDX), the easiest place to capture this
    --        it right here, before we mangle the registers for the probe call we are inserting.
    RX.noAddr $
      RX.makeInstr repr "mov" [ F86.QWordReg F86.RDX
                              , F86.QWordReg F86.RDI
                              ]
  , RX.annotateInstrWith addMemAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RDI
                              , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
  , RX.annotateInstrWith addProbeSupportFnsAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RSI
                              , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
  , RX.noAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RCX
                              , F86.VoidMem (F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
  , RX.annotateInstrWith addJumpTarget $
      RX.makeInstr repr "call" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]
  ]
  ++ generatePops
  where
    -- CHECK: We are pushing an odd number of 8 byte registers here. Will this screw
    -- up stack alignment requirements (16 byte aligned) of X86_64?
    argRegs = [F86.RDI, F86.RSI, F86.RDX, F86.RCX, F86.R8, F86.R9, F86.RAX]
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

-- | Make an instruction sequence to call an exit probe
--
-- This has to restore the environment to exactly what it was before the call.
-- It currently passes the following arguments to the probe (in order):
--  + Pointer to the global storage
--  + An array of support functions that the probe can access
--  + The `arg0` value - in the exit probe, it is the result of the function that was probed
--  + The `ucaller` value - specified to be the address of the probe call instruction
--
-- Note that we rely on renovate's relocation support to post-facto fix up addresses
-- by annotating the corresponding arguments.
callExitProbe
  :: MA.ProbeLocationAnalysisResult globals RX.X86_64
  -- ^ Analysis results we can draw from (including the pointer to global storage)
  -> R.InstructionArchRepr RX.X86_64 tp
  -- ^ The instruction repr (only one for x86_64)
  -> R.SymbolicAddress RX.X86_64
  -- ^ The symbolic address of the injected probe function
  -> [R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64)]
callExitProbe locationAnalysis repr@RX.X86Repr probeAddr =
  generatePushes ++
  [ RX.annotateInstrWith addMemAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RDI
                              , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
  , RX.annotateInstrWith addProbeSupportFnsAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RSI
                              , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
    -- NOTE: The return value should be in RAX
  , RX.noAddr $
      RX.makeInstr repr "mov" [ F86.QWordReg F86.RDX
                              , F86.QWordReg F86.RAX
                              ]
  , RX.noAddr $
      RX.makeInstr repr "lea" [ F86.QWordReg F86.RCX
                              , F86.VoidMem (F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
                              ]
  , RX.annotateInstrWith addJumpTarget $
      RX.makeInstr repr "call" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]
  ]
  ++ generatePops
  where
    -- CHECK: We are pushing an odd number of 8 byte registers here. Will this screw
    -- up stack alignment requirements (16 byte aligned) of X86_64?
    argRegs = [F86.RDI, F86.RSI, F86.RDX, F86.RCX, F86.R8, F86.R9, F86.RAX]
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

matcherEntry
  :: R.SymbolicAddress RX.X86_64
  -> LD.ProbeDescription
  -> [LDP.ProbeComponent]
  -> MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> R.SymbolicBlock RX.X86_64
  -> Maybe (MP.ProbeInserter RX.X86_64)
matcherEntry probeSymAddr _providerName symNames locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  let symAddrs = concatMap (Common.symAddressForSymbolPattern locationAnalysis) symNames
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (lastSymTgt `elem` symAddrs)
    return $ MP.ProbeInserter $ \irep insns ->
      let term DLN.:| rest = DLN.reverse insns
          callSequence = callEntryProbe locationAnalysis irep probeSymAddr
          rinsns = term DLN.:| (reverse callSequence <> rest)
      in DLN.reverse rinsns

matcherExit
  :: R.SymbolicAddress RX.X86_64
  -> LD.ProbeDescription
  -> [LDP.ProbeComponent]
  -> MA.ProbeLocationAnalysisResult globals RX.X86_64
  -> R.SymbolicBlock RX.X86_64
  -> Maybe (MP.ProbeInserter RX.X86_64)
matcherExit probeSymAddr _providerName symNames locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  let symAddrs = concatMap (Common.symAddressForSymbolPattern locationAnalysis) symNames
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (lastSymTgt `elem` symAddrs)
    return $ MP.ProbeInserter $ \irep insns ->
      let callSequence = callExitProbe locationAnalysis irep probeSymAddr
          term DLN.:| rest = insns
      in term DLN.:| (rest <> callSequence)

matchProbes :: MA.ProbeLocationAnalysisResult globals RX.X86_64
            -> R.SymbolicBlock RX.X86_64
            -> [MP.ProbeInserter RX.X86_64]
matchProbes probeLocations symBlock = do
  let probeInserterPairs = mapMaybe (\p -> (p, ) <$> MP.providerMatcher p probeLocations symBlock) allProviders
  map snd (reorder probeInserterPairs)
  where
    allProviders = providerList probeLocations
    -- FIXME: The precise ordering needs more thought and especially how it interacts
    -- with the insertion process in `rewrite`. Consider another classification method as well
    reorder probeInserterPairs =
      let entryProbes = filter (\(p, _) -> MP.isEntry $ probeName p) probeInserterPairs
          exitProbes = filter (\(p, _) -> MP.isReturn $ probeName p) probeInserterPairs
      in entryProbes ++ exitProbes
    probeName p =  LD.probeName (MP.providerName p)


providerList :: MA.ProbeLocationAnalysisResult globals RX.X86_64 -> [MP.ProbeProvider globals RX.X86_64]
providerList probeLocations =
  let assets = MA.injectedAssets probeLocations
  in concatMap makeProviders (MA.injectedProbeAddrs assets)
  where
    makeProviders (LDT.Probe descs _ _ _, symAddr) = map (makeStandardSyscallProvider symAddr) $ DLN.toList descs

makeStandardSyscallProvider ::  R.SymbolicAddress RX.X86_64 -> LDP.ProbeDescription -> MP.ProbeProvider globals RX.X86_64
makeStandardSyscallProvider probeSymAddr probeDesc =
  assert (MP.isEntry probeName || MP.isReturn probeName) provider
  where
    probeName = LDP.probeName probeDesc
    fnName = LDP.probeFunction probeDesc
    patterns = catMaybes [ Just fnName
                         , LDP.appendIdentifier fnName "@plt"
                         ]

    provider = MP.ProbeProvider { MP.providerName = probeDesc
                                , MP.providerDescription = desc
                                , MP.providerMatcher = matcher probeSymAddr probeDesc patterns
                                }
    matcher = if MP.isEntry probeName then matcherEntry else matcherExit
    desc = PP.hsep
        [ PP.pretty ("Probe fires at the" :: String)
        , PP.pretty (if MP.isEntry probeName then "entry to" else "return of" :: String)
        , PP.pretty $ LDP.probeFunction probeDesc
        , PP.pretty ("calls" :: String)
        ]
