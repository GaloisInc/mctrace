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

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as DBU
import           Data.Coerce ( coerce )
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as Map
import           Data.Semigroup ( sconcat )
import           Data.Word ( Word8, Word32 )
import qualified Data.Parameterized.TraversableFC as FC
import           Data.Parameterized.List(List(..))

import qualified Data.Macaw.PPC.PPCReg ()  -- Needed for the instances. TODO: Investigate again
import qualified Dismantle.PPC as D
import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP

import           MCTrace.Arch.PPC.Internal
import qualified MCTrace.Runtime as RT
import MCTrace.Runtime ( probeSupportFunctions, probeSupportFunctionIndexMap )
import GHC.Int (Int16, Int32)

-- | Generate instructions to allocate memory using a support function.
-- A pointer to the allocated memory will be returned in RAX (assuming
-- the support function does the right thing).
allocMemory
  :: R.InstructionArchRepr RP.PPC32 RP.OnlyEncoding
  -> R.SymbolicAddress RP.PPC32
  -> Word32
  -> FilePath
  -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
allocMemory repr allocFnSymAddress globalStoreSize path =
  i (D.Instruction D.LI (gpr 3 :< D.S16imm (fromIntegral globalStoreSize) :< Nil)) DLN.:|
  [ annotateInstrWith addAllocFunAddress $ i (D.Instruction D.BL (D.Calltarget (D.BT 0) :< Nil)) ]
  where
    addAllocFunAddress :: D.Operand x -> D.Annotated (R.Relocation RP.PPC32) D.Operand x
    addAllocFunAddress op =
      case op of
        D.Calltarget _ -> D.Annotated (R.SymbolicRelocation allocFnSymAddress) op
        _ -> D.Annotated R.NoRelocation op

initializeProbeSupportFunArray
  :: R.InstructionArchRepr RP.PPC32 RP.OnlyEncoding
  -> Word32
  -> Map.Map RT.SupportFunction (R.SymbolicAddress RP.PPC32)
  -> R.ConcreteAddress RP.PPC32
  -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
initializeProbeSupportFunArray repr pointerWidth supportFunctions probeSupportFunArrayAddr =
  let probeStoreInstrs = sconcat $ DLN.fromList $ map storeFnAddr probeSupportFunctions
  in baseAddrInstrs <> probeStoreInstrs
  where
    baseAddrInstrs = loadConcreteAddress 8 probeSupportFunArrayAddr
    storeFnAddr fn =
      let symAddr = supportFunctions Map.! fn
          index = probeSupportFunctionIndexMap Map.! fn
      in annotateInstrWith (addSupportFnAddr symAddr) (
           i (D.Instruction D.LA (gpr 7 :< D.S16imm 0 :< gpr_nor0 6 :< Nil))
         ) DLN.:|
         [ i (D.Instruction D.STW (regOffset 8 (displacement index) :< gpr 7 :< Nil)) ]
    addSupportFnAddr :: R.SymbolicAddress RP.PPC32 -> D.Operand x -> D.Annotated (R.Relocation RP.PPC32) D.Operand x
    addSupportFnAddr symAddr op =
      case op of
        D.Gprc_nor0 _ -> D.Annotated (R.SymbolicRelocation symAddr) op
        _ -> D.Annotated R.NoRelocation op
    displacement index = fromIntegral (fromIntegral index * pointerWidth)


-- | The code sequence to initialize the probe execution environment (including global storage)
--
-- This will embed the file path as raw data and then call a runtime support
-- function to allocate the actual memory.
initializationCode
  :: FilePath
  -- ^ The path of a file to serve as the backing store for the probe global storage
  --
  -- This path will be inserted into the instruction stream
  -> Word32
  -- ^ The number of bytes required for the global storage of probes
  -> R.ConcreteAddress RP.PPC32
  -- ^ The address of the global variable that will hold the pointer to the storage area
  -> Map.Map RT.SupportFunction (R.SymbolicAddress RP.PPC32)
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
initializationCode path globalStoreSize globalAddr supportFunctions probeSupportFunArrayAddr repr pointerWidth origEntry =
  withCallerSaveRegisters (
    sconcat (
      allocMemory repr allocMemFnAddress globalStoreSize path DLN.:|
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
  --   allocMemFnAddress = supportFunctions Map.! RT.AllocMemory
  -- neconcat (-- Save the registers we are going to clobber to the stack
  --           applyRegisters repr "push" usedRegisters DLN.:|
  --           [
  --        -- Allocate memory using the external function
  --        -- FIXME: Exit if the call failed. e.g. %rax < 0
  --        allocMemory repr allocMemFnAddress globalStoreSize path

  --        -- Save the address returned by mmap (in %rax) into the global variable
  --        --
  --        -- We need to turn this into a symbolic reference to be fixed up later
  --        , (RP.annotateInstrWith addMemAddr $
  --              RP.makeInstr repr "mov"
  --              [ F86.Mem64 (F86.IP_Offset_32 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
  --              , F86.QWordReg F86.RAX
  --              ]
  --          ) DLN.:| []
  --        -- Initialize the probe support function gloabl array of pointers
  --        , initializeProbeSupportFunArray repr pointerWidth supportFunctions probeSupportFunArrayAddr
  --          -- Restore saved values
  --        , applyRegisters repr "pop" (DLN.reverse usedRegisters)
  --        , (RP.annotateInstrWith addEntryAddr $
  --           RP.makeInstr repr "jmp" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]) DLN.:| []
  --        ])
  -- where
  --   addEntryAddr (RP.AnnotatedOperand v _) =
  --     case v of
  --       (F86.JumpOffset {}, _) -> RP.AnnotatedOperand v (R.PCRelativeRelocation origEntry)
  --       _ -> RP.AnnotatedOperand v R.NoRelocation

  --   addMemAddr (RP.AnnotatedOperand v _) =
  --     case v of
  --       (F86.Mem64 {}, _) -> RP.AnnotatedOperand v (R.PCRelativeRelocation globalAddr)
  --       _ -> RP.AnnotatedOperand v R.NoRelocation
  --   allocMemFnAddress = supportFunctions Map.! RT.AllocMemory

{- Note [System Calls]

On ppc_64 linux:

- The system call number is placed in %rax
- The return value is placed in %rax
- Arguments are placed (in order) in: %rdi, %rsi, %rdx, %r10, %r8, %r9

System calls of note for this:
- write (%rax = 1)
- open (%rax = 2)
- mmap (%rax = 9)
- exit (%rax = 60)
- ftruncate (%rax = 77)

Note that mmap requires all 6 arguments

-}

