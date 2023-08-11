{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module MCTrace.Arch.PPC.Setup (
  initializationCode
  ) where

import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as Map
import           Data.Semigroup ( sconcat )
import           Data.Word ( Word32 )
import           Data.Parameterized.List(List(..))

import qualified Data.Macaw.PPC.PPCReg ()  -- Needed for the instances. TODO: Investigate again
import qualified Dismantle.PPC as D
import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP

import qualified MCTrace.Arch.Common as MAC
import           MCTrace.Arch.PPC.Internal
import qualified MCTrace.RuntimeAPI as RT
import MCTrace.RuntimeAPI ( probeSupportFunctions, probeSupportFunctionIndexMap )

-- | Generate instructions to allocate memory using a support function.
-- A pointer to the allocated memory will be returned in R3 (assuming
-- the support function does the right thing).
allocMemory
  :: R.InstructionArchRepr RP.PPC32 RP.OnlyEncoding
  -> MAC.SymbolLocation RP.PPC32
  -> Word32
  -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
allocMemory _repr allocSymbolLocation globalStoreSize =
  -- Note: Since our call is to a ConcreteAddress + int offset, we cannot fully resolve
  -- this at the time we are inserting these instructions. So, we resort to an indirect
  -- call via a register.
  i (D.Instruction D.LI (gpr 3 :< D.S16imm (fromIntegral globalStoreSize) :< Nil)) DLN.:|
  [ annotateInstrWith addAllocFunAddress $ i (D.Instruction D.LA (gpr 7 :< D.S16imm 0 :< gpr_nor0 6 :< Nil)) 
  , i (D.Instruction D.ADDI (gpr 7 :< D.S16imm (fromIntegral allocFnOffset) :< gpr_nor0 7 :< Nil))
  , i (D.Instruction D.MTCTR (gpr 7 :< Nil))
  , i (D.Instruction D.BCTRL Nil)
  ]
  where
    allocFnSymAddress = MAC.slSymbolBaseAddress allocSymbolLocation
    allocFnOffset = MAC.slSymbolOffset allocSymbolLocation
    addAllocFunAddress :: D.Operand x -> D.Annotated (R.Relocation RP.PPC32) D.Operand x
    addAllocFunAddress op =
      case op of
        D.Gprc_nor0 _ -> D.Annotated (R.SymbolicRelocation allocFnSymAddress) op
        _ -> D.Annotated R.NoRelocation op

initializeProbeSupportFunArray
  :: R.InstructionArchRepr RP.PPC32 RP.OnlyEncoding
  -> Word32
  -> Map.Map RT.SupportFunction (MAC.SymbolLocation RP.PPC32)
  -> R.ConcreteAddress RP.PPC32
  -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
initializeProbeSupportFunArray _repr pointerWidth supportFunctions probeSupportFunArrayAddr =
  let probeStoreInstrs = sconcat $ DLN.fromList $ map storeFnAddr probeSupportFunctions
  in baseAddrInstrs <> probeStoreInstrs
  where
    baseAddrInstrs = loadConcreteAddress 8 probeSupportFunArrayAddr
    storeFnAddr fn =
      let symbolLocation = supportFunctions Map.! fn
          symAddr = MAC.slSymbolBaseAddress symbolLocation
          symOffset = MAC.slSymbolOffset symbolLocation
          index = probeSupportFunctionIndexMap Map.! fn
      in annotateInstrWith (addSupportFnAddr symAddr) (
           i (D.Instruction D.LA (gpr 7 :< D.S16imm 0 :< gpr_nor0 6 :< Nil))
         ) DLN.:|
         [ i (D.Instruction D.ADDI (gpr 7 :< D.S16imm (fromIntegral symOffset) :< gpr_nor0 7 :< Nil))
         , i (D.Instruction D.STW (regOffset 8 (displacement index) :< gpr 7 :< Nil)) 
         ]
    addSupportFnAddr :: R.SymbolicAddress RP.PPC32 -> D.Operand x -> D.Annotated (R.Relocation RP.PPC32) D.Operand x
    addSupportFnAddr symAddr op =
      case op of
        D.Gprc_nor0 _ -> D.Annotated (R.SymbolicRelocation symAddr) op
        _ -> D.Annotated R.NoRelocation op
    displacement index = fromIntegral (fromIntegral index * pointerWidth)


-- | The code sequence to initialize the probe execution environment (including global storage)
--
-- This will embed the file path as raw data and then call a platform
-- API function to allocate the actual memory.
initializationCode
  :: Word32
  -- ^ The number of bytes required for the global storage of probes
  -> R.ConcreteAddress RP.PPC32
  -- ^ The address of the global variable that will hold the pointer to the storage area
  -> Map.Map RT.SupportFunction (MAC.SymbolLocation RP.PPC32)
  -- ^ The symbolic addresses assigned to each support function we injected
  -> R.ConcreteAddress RP.PPC32
  -- ^ The address of the global variable that will hold array of probe accessible function addresses
  -> R.InstructionArchRepr RP.PPC32 RP.OnlyEncoding
  -- ^ The repr indicating which instruction set to use
  -> Word32
  -- ^ Width of a pointer on this architecture
  -> R.ConcreteAddress RP.PPC32
  -- ^ The original entry point to the program
  -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
initializationCode globalStoreSize globalAddr supportFunctions probeSupportFunArrayAddr repr pointerWidth origEntry =
  withCallerSaveRegisters (
    sconcat (
      allocMemory repr allocMemFnAddress globalStoreSize DLN.:|
      [ loadConcreteAddress 9 globalAddr
      , il (D.Instruction D.STW (regOffset 9 0 :< gpr 3 :< Nil))
      , initializeProbeSupportFunArray repr pointerWidth supportFunctions probeSupportFunArrayAddr
      ]
    )
  ) <> ( annotateInstrWith addEntryAddr <$> il (D.Instruction D.B (D.Directbrtarget (D.mkBranchTarget 0) :< Nil)) )
  where
    allocMemFnAddress = supportFunctions Map.! RT.AllocMemory
    addEntryAddr :: D.Operand x -> D.Annotated (R.Relocation RP.PPC32) D.Operand x
    addEntryAddr op =
      case op of
        D.Directbrtarget _ -> D.Annotated (R.SymbolicRelocation (R.stableAddress origEntry)) op
        _ -> D.Annotated R.NoRelocation op


