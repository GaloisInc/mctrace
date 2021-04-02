{-# LANGUAGE TypeInType #-}
module MCTrace.Arch.X86.Setup (
  linuxInitializationCode
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as DBU
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import           Data.Word ( Word32 )

import qualified Flexdis86 as F86
import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as RX

-- syscallWrite :: F86.Value
-- syscallWrite = F86.DWordSignedImm 1
syscallOpen :: F86.Value
syscallOpen = F86.DWordSignedImm 2
syscallMmap :: F86.Value
syscallMmap = F86.DWordSignedImm 9
-- syscallExit :: F86.Value
-- syscallExit = F86.DWordSignedImm 60
syscallFtruncate :: F86.Value
syscallFtruncate = F86.DWordSignedImm 77

i :: RX.Instruction tp () -> R.TaggedInstruction RX.X86_64 tp RX.TargetAddress
i = R.tagInstruction Nothing . RX.noAddr

applyRegisters
  :: Functor f
  => RX.X86Repr tp
  -> String
  -> f F86.Value
  -> f (R.TaggedInstruction RX.X86_64 tp RX.TargetAddress)
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

-- | Issue an open syscall: open(path, O_CREAT | O_RDWR | O_TRUNC, 0400 | 0200)
--
-- open is syscall 2
--
-- The flags are: 0100 | 02 | 01000 = 578
--
-- The mode is 0400 | 0200 = 384
--
-- This code sequence stores the bytes of the path inline. It uses an
-- unconditional jump to avoid executing it.
open
  :: R.InstructionArchRepr RX.X86_64 tp
  -> FilePath
  -> DLN.NonEmpty (R.TaggedInstruction RX.X86_64 tp (R.InstructionAnnotation RX.X86_64))
open repr path =
    i (RX.makeInstr repr "mov" [ F86.QWordReg F86.RAX
                               , syscallOpen
                               ]) DLN.:|
  [ i $ RX.makeInstr repr "lea" [ F86.QWordReg F86.RDI
                                , F86.VoidMem (F86.IP_Offset_64 F86.SS (F86.Disp32 (F86.Imm32Concrete 0x15)))
                                ]
  , i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.RSI
                                , F86.DWordSignedImm 578
                                ]
  , i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.RDX
                                , F86.DWordSignedImm 384
                                ]
  , i $ RX.makeInstr repr "syscall" []
  , i $ RX.makeInstr repr "jmp" [F86.JumpOffset F86.JSize32 (F86.FixedOffset (fromIntegral jmpOff))]
  , i $ RX.rawBytes repr (DBU.fromString path <> BS.pack [0])
  ]
  where
    -- This is the offset to the jump over the file path
    --
    -- We add 1 for the NUL terminator
    jmpOff = length path + 1

-- | Truncate the file up to the necessary size: ftruncate(fd, size)
--
-- Note that the FD is already in %rdi, so we just need to populate the size
ftruncate
  :: R.InstructionArchRepr RX.X86_64 tp
  -> Word32
  -> DLN.NonEmpty (R.TaggedInstruction RX.X86_64 tp (R.InstructionAnnotation RX.X86_64))
ftruncate repr globalStoreBytes =
    i (RX.makeInstr repr "mov" [ F86.QWordReg F86.RAX
                               , syscallFtruncate
                               ]) DLN.:|
  [ i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.RSI
                                , F86.DWordSignedImm (fromIntegral globalStoreBytes)
                                ]
  , i $ RX.makeInstr repr "syscall" []
  ]

-- | Map the file descriptor (in %r8)
--
-- * We do not request a specific address
-- * The protection flags are PROT_READ | PROT_WRITE = 0x1 | 0x2 = 0x3
-- * The flags are MAP_SHARED = 0x1
-- * We do not use an offset into the file
mmap
  :: R.InstructionArchRepr RX.X86_64 tp
  -> Word32
  -> DLN.NonEmpty (R.TaggedInstruction RX.X86_64 tp (R.InstructionAnnotation RX.X86_64))
mmap repr globalStoreBytes =
    i (RX.makeInstr repr "mov" [ F86.QWordReg F86.RAX, syscallMmap ]) DLN.:|
   -- Pass NULL as the requested address
  [ i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.RDI, F86.DWordSignedImm 0 ]
   -- The number of bytes in the mapping
  , i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.RSI, F86.DWordSignedImm (fromIntegral globalStoreBytes) ]
   -- Protection flags
  , i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.RDX, F86.DWordSignedImm  3 ]
   -- The flags
  , i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.R10, F86.DWordSignedImm 1 ]
   -- The offset into the file (0)
  , i $ RX.makeInstr repr "mov" [ F86.QWordReg F86.R9, F86.DWordSignedImm 0 ]
  , i $ RX.makeInstr repr "syscall" []
  ]

neconcat :: DLN.NonEmpty (DLN.NonEmpty a) -> DLN.NonEmpty a
neconcat nel =
  case nel of
    (e0 DLN.:| rest) DLN.:| others ->
      e0 DLN.:| (rest <> concat (fmap F.toList others))

-- | The code sequence to initialize the probe execution environment (including global storage)
--
-- This will embed the file path as raw data and then use the @open@ and @mmap@
-- system calls to set up a memory region backed by a file.
--
-- See Note [System Calls] for details on calling conventions
linuxInitializationCode
  :: FilePath
  -- ^ The path of a file to serve as the backing store for the probe global storage
  --
  -- This path will be inserted into the instruction stream
  -> Word32
  -- ^ The number of bytes required for the global storage of probes
  -> R.ConcreteAddress RX.X86_64
  -- ^ The address of the global variable that will hold the pointer to the storage area
  -> R.InstructionArchRepr RX.X86_64 tp
  -- ^ The repr indicating which instruction set to use
  -> R.ConcreteAddress RX.X86_64
  -- ^ The original entry point to the program
  -> DLN.NonEmpty (R.TaggedInstruction RX.X86_64 tp (R.InstructionAnnotation RX.X86_64))
linuxInitializationCode path globalStoreBytes globalAddr repr origEntry =
  neconcat (-- Save the registers we are going to clobber to the stack
            applyRegisters repr "push" usedRegisters DLN.:|
            [
         -- FIXME: Exit if %rax < 0
              open repr path
         -- Save the FD into %rdi (as an argument to the next syscall)
         , i (RX.makeInstr repr "mov" [ F86.QWordReg F86.RDI, F86.QWordReg F86.RAX ]) DLN.:| []
         , ftruncate repr globalStoreBytes
         -- Move the FD to %r8 (as the fifth argument of mmap)
         , i (RX.makeInstr repr "mov" [ F86.QWordReg F86.R8, F86.QWordReg F86.RDI ]) DLN.:| []
         , mmap repr globalStoreBytes
         -- Save the address returned by mmap (in %rax) into the global variable
         --
         -- We need to turn this into a symbolic reference to be fixed up later
         , (R.tagInstruction Nothing $
               RX.annotateInstrWith addMemAddr $
               RX.makeInstr repr "mov"
               [ F86.Mem64 (F86.IP_Offset_32 F86.SS (F86.Disp32 (F86.Imm32Concrete 0)))
               , F86.QWordReg F86.RAX
               ]
           ) DLN.:| []
           -- Restore saved values
         , applyRegisters repr "pop" (DLN.reverse usedRegisters)
         , (R.tagInstruction Nothing $
            RX.annotateInstrWith addEntryAddr $
            RX.makeInstr repr "jmp" [F86.JumpOffset F86.JSize32 (F86.FixedOffset 0)]) DLN.:| []
         ])
  where
    addEntryAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.JumpOffset {}, _) -> RX.AnnotatedOperand v (RX.AbsoluteAddress origEntry)
        _ -> RX.AnnotatedOperand v RX.NoAddress

    addMemAddr (RX.AnnotatedOperand v _) =
      case v of
        (F86.Mem64 {}, _) -> RX.AnnotatedOperand v (RX.AbsoluteAddress globalAddr)
        _ -> RX.AnnotatedOperand v RX.NoAddress

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
