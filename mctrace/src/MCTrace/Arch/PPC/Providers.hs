{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
-- | Definitions of probe providers for ppc_64
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
module MCTrace.Arch.PPC.Providers (
  matchProbes
  ) where

import           Control.Monad ( guard )
import           Control.Exception ( assert )
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as Map
import           Data.Maybe ( mapMaybe )
import           Data.Parameterized.List(List(..))
import           Data.Semigroup ( sconcat )
import qualified Data.Text as T

import qualified Prettyprinter as PP
import qualified Dismantle.PPC as D
import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT
import qualified Language.DTrace.ProbeDescription as LDP
import           MCTrace.Arch.PPC.Internal
import qualified MCTrace.Analysis as MA
import qualified MCTrace.ProbeProvider as MP

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
  :: MA.ProbeLocationAnalysisResult globals RP.PPC32
  -- ^ Analysis results we can draw from (including the pointer to global storage)
  -> R.InstructionArchRepr RP.PPC32 tp
  -- ^ The instruction repr (only one for ppc_64)
  -> R.SymbolicAddress RP.PPC32
  -- ^ The symbolic address of the injected probe function
  -> DLN.NonEmpty (R.Instruction RP.PPC32 tp (R.Relocation RP.PPC32))
callProbe locationAnalysis RP.PPCRepr probeAddr =
  withCallerSaveRegisters $ 
    -- withSavedLinkRegister $
      sconcat ( 
        loadConcreteAddress 3 globalStorePtr DLN.:|
        [ loadConcreteAddress 4 probeSupportFnsPtr
        , computeUCaller
        , annotateInstrWith addProbeFunAddress <$> il (D.Instruction D.BL (D.Calltarget (D.BT 0) :< Nil))
        ]
      )
  where
    globalStorePtr = MA.injectedStorePointer (MA.injectedAssets locationAnalysis)
    probeSupportFnsPtr = MA.probeSupportFunctionsPtr (MA.injectedAssets locationAnalysis)
    addProbeFunAddress :: D.Operand x -> D.Annotated (R.Relocation RP.PPC32) D.Operand x
    addProbeFunAddress op =
      case op of
        D.Calltarget _ -> D.Annotated (R.SymbolicRelocation probeAddr) op
        _ -> D.Annotated R.NoRelocation op
    computeUCaller =
      -- There is no *direct* way to get hold of the current PC on PPC as far
      -- as I am aware or otherwise perform PC-relative addressing without
      -- involving a call/branch. So, the idea here is create a call to the
      -- very next instruction, capture the Link Register (LR) and then tweak it
      -- to point to actual probe call instruction (which we have chosen as the
      -- value we will return for ucaller on all architectures)

      -- NOTES:
      --  1. Note that branch target values/offsets are left shifted by 2 to
      --     compute the actual branch target. So using 1 as the offset actually
      --     means 4, i.e. the next instruction
      --  2. We are actually not saving/restoring the LR during this process as
      --     it has already been saved in the preamble code we are inserting for
      --     this probe call. So, we can afford to change its value without
      --     additional work.
      --  2. IMPORTANT: There is an implicit assumption that `ucaller` is the last
      --     parameter to the probe as this simplifies the calculation needed to
      --     tweak the captured LR to point to the probe call instruction.
      i (D.Instruction D.BL (D.Calltarget (D.BT 1) :< Nil)) DLN.:|
      [ i (D.Instruction D.MFLR (gpr 5 :< Nil)) 
      , i (D.Instruction D.ADDI (gpr 5 :< D.S16imm 8 :< gpr_nor0 5 :< Nil))
      ]

  -- generatePushes ++
  -- [ RP.annotateInstrWith addMemAddr $
  --     RP.makeInstr repr "lea" [ F86.QWordReg F86.RDI
  --                             , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
  --                             ]
  -- , RP.annotateInstrWith addProbeSupportFnsAddr $
  --     RP.makeInstr repr "lea" [ F86.QWordReg F86.RSI
  --                             , F86.VoidMem(F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
  --                             ]
  -- , RP.noAddr $
  --     RP.makeInstr repr "lea" [ F86.QWordReg F86.RDX
  --                             , F86.VoidMem (F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
  --                             ]
  -- , RP.annotateInstrWith addJumpTarget $
  --     RP.makeInstr repr "call" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]
  -- ]
  -- ++ generatePops
  -- where
  --   -- CHECK: We are pushing an odd number of 8 byte registers here. Will this screw
  --   -- up stack alignment requirements (16 byte aligned) of PPC?
  --   argRegs = [F86.RDI, F86.RSI, F86.RDX, F86.RCX, F86.R8, F86.R9, F86.RAX]
  --   generatePushes = map (\r -> RP.noAddr $ RP.makeInstr repr "push" [F86.QWordReg r]) argRegs
  --   generatePops = map (\r -> RP.noAddr $ RP.makeInstr repr "pop" [F86.QWordReg r]) (reverse argRegs)
  --   addJumpTarget (RP.AnnotatedOperand v _) =
  --     case v of
  --       (F86.JumpOffset {}, _) -> RP.AnnotatedOperand v (R.SymbolicRelocation probeAddr)
  --       _ -> RP.AnnotatedOperand v R.NoRelocation
  --   addMemAddr (RP.AnnotatedOperand v _) =
  --     case v of
  --       (F86.VoidMem {}, _) -> RP.AnnotatedOperand v (R.PCRelativeRelocation globalStorePtr)
  --       _ -> RP.AnnotatedOperand v R.NoRelocation
  --   addProbeSupportFnsAddr (RP.AnnotatedOperand v _) =
  --     case v of
  --       (F86.VoidMem {}, _) -> RP.AnnotatedOperand v (R.PCRelativeRelocation probeSupportFnsPtr)
  --       _ -> RP.AnnotatedOperand v R.NoRelocation
  --   globalStorePtr = MA.injectedStorePointer (MA.injectedAssets locationAnalysis)
  --   probeSupportFnsPtr = MA.probeSupportFunctionsPtr (MA.injectedAssets locationAnalysis)

-- | If the last instruction is a call or a jump, return its symbolic target
--
-- On ppc, it happens to be the case that all jump/call instructions just take a
-- single operand, which will have an associated relocation at this stage.
withLastInstructionSymTarget
  :: a
  -> R.SymbolicBlock RP.PPC32
  -> (R.SymbolicAddress RP.PPC32 -> a)
  -> a
withLastInstructionSymTarget def symBlock k = R.withSymbolicInstructions symBlock $ \_repr insns -> do
  case RP.toAnnotatedInst (DLN.head (DLN.reverse insns)) of
    D.Instruction _opc operands ->
      case operands of
        D.Annotated (R.SymbolicRelocation symaddr) (D.Calltarget _) :< Nil ->
          k symaddr
        D.Annotated (R.SymbolicRelocation symaddr) (D.Directbrtarget _) :< Nil ->
          k symaddr
        -- TODO: Is it possible to also get D.Condbrtarget ?
        _ -> def

symAddressForSymbolPattern
  :: MA.ProbeLocationAnalysisResult globals RP.PPC32
  -> String
  -> [R.SymbolicAddress RP.PPC32]
symAddressForSymbolPattern locationAnalysis symPattern = do
  -- Find all (symbolic addresses of) all functions in the binary that match the given pattern
  let relevantFns = Map.filterWithKey (checkForMatch symPattern) (MA.functionEntryPoints locationAnalysis)
      lookupSymbolicAddr entryAddr = Map.findWithDefault (R.stableAddress entryAddr) entryAddr (MA.symbolicAddresses locationAnalysis)
  map lookupSymbolicAddr $ Map.elems relevantFns
  where
    checkForMatch pattern fnName _ = LDP.matchWithPattern (T.pack pattern) (T.pack $ BSC.unpack fnName)


matcherEntry
  :: R.SymbolicAddress RP.PPC32
  -> LD.ProbeDescription
  -> [String]
  -> MA.ProbeLocationAnalysisResult globals RP.PPC32
  -> R.SymbolicBlock RP.PPC32
  -> Maybe (MP.ProbeInserter RP.PPC32)
matcherEntry probeSymAddr _providerName symNames locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  let symAddrs = concatMap (symAddressForSymbolPattern locationAnalysis) symNames
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (lastSymTgt `elem` symAddrs)
    return $ MP.ProbeInserter $ \irep insns ->
      let term DLN.:| rest = DLN.reverse insns
          callSequence = DLN.toList $ callProbe locationAnalysis irep probeSymAddr
          rinsns = term DLN.:| (reverse callSequence <> rest)
      in DLN.reverse rinsns

matcherExit
  :: R.SymbolicAddress RP.PPC32
  -> LD.ProbeDescription
  -> [String]
  -> MA.ProbeLocationAnalysisResult globals RP.PPC32
  -> R.SymbolicBlock RP.PPC32
  -> Maybe (MP.ProbeInserter RP.PPC32)
matcherExit probeSymAddr _providerName symNames locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  let symAddrs = concatMap (symAddressForSymbolPattern locationAnalysis) symNames
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (lastSymTgt `elem` symAddrs)
    return $ MP.ProbeInserter $ \irep insns ->
      -- FIXME: Currently the instructions to insert the probe are the same, irrespective
      -- of whether it is at entry or exit. This is likely to change in the future. So, it
      -- may be good to have a separate version of `callProbe` for exit probes.
      let callSequence = DLN.toList $ callProbe locationAnalysis irep probeSymAddr
          term DLN.:| rest = insns
      in term DLN.:| (rest <> callSequence)

matchProbes :: MA.ProbeLocationAnalysisResult globals RP.PPC32
            -> R.SymbolicBlock RP.PPC32
            -> [MP.ProbeInserter RP.PPC32]
matchProbes probeLocations symBlock = do
  let probeInserterPairs = mapMaybe (\p -> (p, ) <$> MP.providerMatcher p probeLocations symBlock) allProviders
  map snd (reorder probeInserterPairs)
  where
    allProviders = providerList probeLocations
    -- FIXME: The precise ordering needs more thought and especially how it interacts
    -- with the insertion process in `rewrite`. Consider another classification method as well
    reorder probeInserterPairs =
      let entryProbes = filter (\(p, _) -> probeName p == T.pack "entry") probeInserterPairs
          exitProbes = filter (\(p, _) -> probeName p == T.pack "return") probeInserterPairs
      in entryProbes ++ exitProbes
    probeName p =  LD.probeName (MP.providerName p)


providerList :: MA.ProbeLocationAnalysisResult globals RP.PPC32 -> [MP.ProbeProvider globals RP.PPC32]
providerList probeLocations =
  let assets = MA.injectedAssets probeLocations
  in concatMap makeProviders (MA.injectedProbeAddrs assets)
  where
    makeProviders (LDT.Probe descs _ _ _, symAddr) = map (makeStandardSyscallProvider symAddr) $ DLN.toList descs

makeStandardSyscallProvider ::  R.SymbolicAddress RP.PPC32 -> LDP.ProbeDescription -> MP.ProbeProvider globals RP.PPC32
makeStandardSyscallProvider probeSymAddr probeDesc =
  assert (probeName == T.pack "entry" || probeName == T.pack "return") provider
  where
    fnName = LDP.probeFunction probeDesc
    probeName = LDP.probeName probeDesc
    provider = MP.ProbeProvider { MP.providerName = probeDesc
                                , MP.providerDescription = desc
                                , MP.providerMatcher = matcher probeSymAddr probeDesc [T.unpack fnName, T.unpack fnName ++ "@plt"]
                                }
    matcher = if probeName == T.pack "entry" then matcherEntry else matcherExit
    desc = PP.hsep [ PP.pretty "Probe fires at the"
                   , PP.pretty (if probeName == T.pack "entry" then "entry to" else "return of")
                   , PP.pretty (LDP.probeFunction probeDesc)
                   , PP.pretty "calls"
                   ]
