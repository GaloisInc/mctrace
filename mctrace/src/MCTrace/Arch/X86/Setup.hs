{-# LANGUAGE TypeInType #-}
module MCTrace.Arch.X86.Setup (
  linuxInitializationCode
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as DBU
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as Map
import           Data.Word ( Word32 )

import qualified Flexdis86 as F86
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as RX

import qualified MCTrace.RuntimeAPI as RT
import MCTrace.RuntimeAPI ( probeSupportFunctions, probeSupportFunctionIndexMap )

i :: RX.Instruction tp () -> R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64)
i = RX.noAddr

applyRegisters
  :: Functor f
  => RX.X86Repr tp
  -> String
  -> f F86.Value
  -> f (R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64))
applyRegisters repr mnemonic regs =
  fmap (\r -> i (RX.makeInstr repr mnemonic [r])) regs

usedRegisters :: DLN.NonEmpty F86.Value
usedRegisters =   F86.QWordReg F86.RAX DLN.:|
                [ F86.QWordReg F86.RDI
                , F86.QWordReg F86.RSI
                , F86.QWordReg F86.RDX
                , F86.QWordReg F86.R10
                , F86.QWordReg F86.R8
                , F86.QWordReg F86.R9
                ]

-- | Generate instructions to allocate memory using a support function.
-- A pointer to the allocated memory will be returned in RAX (assuming
-- the support function does the right thing).
allocMemory
  :: R.InstructionArchRepr RX.X86_64 tp
  -> R.SymbolicAddress RX.X86_64
  -> Word32
  -> DLN.NonEmpty (R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64))
allocMemory repr allocFnSymAddress globalStoreSize =
    i (RX.makeInstr repr "mov" [ F86.QWordReg F86.RDI
                               , F86.DWordSignedImm (fromIntegral globalStoreSize)
                               ] ) DLN.:|
  [ RX.annotateInstrWith addAllocFunAddress $
      RX.makeInstr repr "call" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]
  ]
  where
    addAllocFunAddress (RX.AnnotatedOperand v _) =
      case v of
        (F86.JumpOffset {}, _) -> RX.AnnotatedOperand v (R.SymbolicRelocation allocFnSymAddress)
        _ -> RX.AnnotatedOperand v R.NoRelocation

initializeProbeSupportFunArray
  :: R.InstructionArchRepr RX.X86_64 tp
  -> Word32
  -> Map.Map RT.SupportFunction (R.SymbolicAddress RX.X86_64)
  -> R.ConcreteAddress RX.X86_64
  -> DLN.NonEmpty (R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64))
initializeProbeSupportFunArray repr pointerWidth supportFunctions probeSupportFunArrayAddr =
  baseAddrInstr DLN.:| concatMap storeFnAddr probeSupportFunctions
  where
    baseAddrInstr =  RX.annotateInstrWith annotateRootAddr $ RX.makeInstr repr "lea"
                     [ F86.QWordReg F86.RDI
                     , F86.VoidMem (F86.IP_Offset_64 F86.DS (F86.Disp32 (F86.Imm32Concrete 0)))
                     ]
    annotateRootAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.VoidMem {}, _) -> RX.AnnotatedOperand v (R.PCRelativeRelocation probeSupportFunArrayAddr)
        _ -> RX.AnnotatedOperand v R.NoRelocation
    storeFnAddr fn =
      let symAddr = supportFunctions Map.! fn
          index = probeSupportFunctionIndexMap Map.! fn
      in [ RX.annotateInstrWith (addSupportFnAddr symAddr) $ RX.makeInstr repr "lea"
           [ F86.QWordReg F86.R9
           , F86.VoidMem (F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0xa)))
           ]
         , i $ RX.makeInstr repr "mov"
           [ F86.Mem64 (F86.Addr_64 F86.DS (Just F86.RDI) Nothing (F86.Disp32 (F86.Imm32Concrete (displacement index))))
           , F86.QWordReg F86.R9
           ]
         ]
    addSupportFnAddr symAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.VoidMem {}, _) -> RX.AnnotatedOperand v (R.SymbolicRelocation symAddr)
        _ -> RX.AnnotatedOperand v R.NoRelocation
    displacement index = fromIntegral (fromIntegral index * pointerWidth)

neconcat :: DLN.NonEmpty (DLN.NonEmpty a) -> DLN.NonEmpty a
neconcat nel =
  case nel of
    (e0 DLN.:| rest) DLN.:| others ->
      e0 DLN.:| (rest <> concat (fmap F.toList others))

-- | The code sequence to initialize the probe execution environment (including global storage)
--
-- This will embed the file path as raw data and then call a runtime support
-- function to allocate the actual memory.
linuxInitializationCode
  :: Word32
  -- ^ Global store size
  -> R.ConcreteAddress RX.X86_64
  -- ^ The address of the global variable that will hold the pointer to the storage area
  -> Map.Map RT.SupportFunction (R.SymbolicAddress RX.X86_64)
  -- ^ The symbolic addresses assigned to each support function we injected
  -> R.ConcreteAddress RX.X86_64
  -- ^ The address of the global variable that will hold array of probe accessible function addresses
  -> R.InstructionArchRepr RX.X86_64 tp
  -- ^ The repr indicating which instruction set to use
  -> Word32
  -- ^ Width of a pointer on this architecture
  -> R.ConcreteAddress RX.X86_64
  -- ^ The original entry point to the program
  -> DLN.NonEmpty (R.Instruction RX.X86_64 tp (R.Relocation RX.X86_64))
linuxInitializationCode globalStoreSize globalAddr supportFunctions probeSupportFunArrayAddr repr pointerWidth origEntry =
  neconcat (-- Save the registers we are going to clobber to the stack
            applyRegisters repr "push" usedRegisters DLN.:|
            [
         -- Allocate memory using the external function
         -- FIXME: Exit if the call failed. e.g. %rax < 0
         allocMemory repr allocMemFnAddress globalStoreSize

         -- Save the address returned by mmap (in %rax) into the global variable
         --
         -- We need to turn this into a symbolic reference to be fixed up later
         , (RX.annotateInstrWith addMemAddr $
               RX.makeInstr repr "mov"
               [ F86.Mem64 (F86.IP_Offset_32 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
               , F86.QWordReg F86.RAX
               ]
           ) DLN.:| []
         -- Initialize the probe support function gloabl array of pointers
         , initializeProbeSupportFunArray repr pointerWidth supportFunctions probeSupportFunArrayAddr
           -- Restore saved values
         , applyRegisters repr "pop" (DLN.reverse usedRegisters)
         , (RX.annotateInstrWith addEntryAddr $
            RX.makeInstr repr "jmp" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]) DLN.:| []
         ])
  where
    addEntryAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.JumpOffset {}, _) -> RX.AnnotatedOperand v (R.PCRelativeRelocation origEntry)
        _ -> RX.AnnotatedOperand v R.NoRelocation

    addMemAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.Mem64 {}, _) -> RX.AnnotatedOperand v (R.PCRelativeRelocation globalAddr)
        _ -> RX.AnnotatedOperand v R.NoRelocation
    allocMemFnAddress = supportFunctions Map.! RT.AllocMemory

{- Note [System Calls]

On x86_64 linux:

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
