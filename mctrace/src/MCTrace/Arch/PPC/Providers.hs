{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.List.NonEmpty as DLN
import           Data.Maybe ( mapMaybe, catMaybes )
import           Data.Parameterized.List(List(..))
import           Data.Semigroup ( sconcat )

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
  :: MA.ProbeLocationAnalysisResult globals RP.PPC32
  -- ^ Analysis results we can draw from (including the pointer to global storage)
  -> R.InstructionArchRepr RP.PPC32 tp
  -- ^ The instruction repr (only one for ppc_64)
  -> R.SymbolicAddress RP.PPC32
  -- ^ The symbolic address of the injected probe function
  -> DLN.NonEmpty (R.Instruction RP.PPC32 tp (R.Relocation RP.PPC32))
callEntryProbe locationAnalysis RP.PPCRepr probeAddr =
  withCallerSaveRegisters $
    -- withSavedLinkRegister $
      sconcat (
        -- NOTE: Computing the arg0 has to be done first before we mangle
        --       registers for the call to be probe.
        computeArg0 DLN.:|
        [ loadConcreteAddress 3 globalStorePtr
        , loadConcreteAddress 4 probeSupportFnsPtr
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
    computeArg0 =
      -- The first argument to the function being probed (`arg0`) needs to be passed to the
      -- probe. The argument can be found in r3 (if we do this before we mangle the registers).
      -- The probe expects this to be the third argument (i.e. in register r5)
      -- IMPORTANT: Make sure this is invoked before we mangle the registers
      il (D.Instruction D.ORI (gpr 5 :< D.U16imm 0 :< gpr 3 :< Nil))
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
      [ i (D.Instruction D.MFLR (gpr 6 :< Nil))
      , i (D.Instruction D.ADDI (gpr 6 :< D.S16imm 8 :< gpr_nor0 6 :< Nil))
      ]

-- | Make an instruction sequence to call an entry probe
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
  :: MA.ProbeLocationAnalysisResult globals RP.PPC32
  -- ^ Analysis results we can draw from (including the pointer to global storage)
  -> R.InstructionArchRepr RP.PPC32 tp
  -- ^ The instruction repr (only one for ppc_64)
  -> R.SymbolicAddress RP.PPC32
  -- ^ The symbolic address of the injected probe function
  -> DLN.NonEmpty (R.Instruction RP.PPC32 tp (R.Relocation RP.PPC32))
callExitProbe locationAnalysis RP.PPCRepr probeAddr =
  withCallerSaveRegisters $
    -- withSavedLinkRegister $
      sconcat (
        -- NOTE: Computing the arg0 has to be done first before we mangle
        --       registers for the call to be probe.
        computeArg0 DLN.:|
        [ loadConcreteAddress 3 globalStorePtr
        , loadConcreteAddress 4 probeSupportFnsPtr
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
    computeArg0 =
      -- In the return probe, `arg0` is the result of the function being probed.
      -- The value can be found in r3, if we do this before we mangle the registers.
      -- The probe expects this to be the third argument (i.e. in register r5)
      -- IMPORTANT: Make sure this is invoked before we mangle the registers
      il (D.Instruction D.ORI (gpr 5 :< D.U16imm 0 :< gpr 3 :< Nil))
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
      [ i (D.Instruction D.MFLR (gpr 6 :< Nil))
      , i (D.Instruction D.ADDI (gpr 6 :< D.S16imm 8 :< gpr_nor0 6 :< Nil))
      ]

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

matcherEntry
  :: R.SymbolicAddress RP.PPC32
  -> LD.ProbeDescription
  -> [LDP.ProbeComponent]
  -> MA.ProbeLocationAnalysisResult globals RP.PPC32
  -> R.SymbolicBlock RP.PPC32
  -> Maybe (MP.ProbeInserter RP.PPC32)
matcherEntry probeSymAddr _providerName symPatterns locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  let symAddrs = concatMap (Common.symAddressForSymbolPattern locationAnalysis) symPatterns
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (lastSymTgt `elem` symAddrs)
    return $ MP.ProbeInserter $ \irep insns ->
      let term DLN.:| rest = DLN.reverse insns
          callSequence = DLN.toList $ callEntryProbe locationAnalysis irep probeSymAddr
          rinsns = term DLN.:| (reverse callSequence <> rest)
      in DLN.reverse rinsns

matcherExit
  :: R.SymbolicAddress RP.PPC32
  -> LD.ProbeDescription
  -> [LDP.ProbeComponent]
  -> MA.ProbeLocationAnalysisResult globals RP.PPC32
  -> R.SymbolicBlock RP.PPC32
  -> Maybe (MP.ProbeInserter RP.PPC32)
matcherExit probeSymAddr _providerName symPatterns locationAnalysis symBlock = do
  -- symbolic blocks have symbolic jump targets annotated on instructions;
  -- if the last one points to one of the functions we are looking for, we
  -- can fire the probe.
  --
  let symAddrs = concatMap (Common.symAddressForSymbolPattern locationAnalysis) symPatterns
  withLastInstructionSymTarget Nothing symBlock $ \lastSymTgt -> do
    guard (lastSymTgt `elem` symAddrs)
    return $ MP.ProbeInserter $ \irep insns ->
      let callSequence = DLN.toList $ callExitProbe locationAnalysis irep probeSymAddr
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
      let entryProbes = filter (\(p, _) -> MP.isEntry $ probeName p) probeInserterPairs
          exitProbes = filter (\(p, _) -> MP.isReturn $ probeName p) probeInserterPairs
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
        [ PP.pretty ("Probe fires at the"::String)
        , PP.pretty (if MP.isEntry probeName then "entry to" else "return of"::String)
        , PP.pretty $ LDP.probeFunction probeDesc
        , PP.pretty ("calls"::String)
        ]
