{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module MCTrace.Arch.PPC.Internal (
  makeInstr, i, il, annotateInstrWith, 
  gpr, gpr_nor0, regOffset, loadImm32,
  loadConcreteAddress, withCallerSaveRegisters
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
import qualified Data.Macaw.PPC as MP
import qualified Dismantle.PPC as D
import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP

import GHC.Int (Int16, Int32)

-- makeInstr = R.fromGenericInstruction @RP.PPC32 RP.PPCRepr
makeInstr :: D.Instruction -> RP.Instruction RP.OnlyEncoding ()
makeInstr = RP.fromInst RP.PPCRepr

i :: D.Instruction -> RP.Instruction RP.OnlyEncoding (R.Relocation RP.PPC32)
i instr = fmap (const R.NoRelocation) (makeInstr instr)

il :: D.Instruction -> DLN.NonEmpty (RP.Instruction RP.OnlyEncoding (R.Relocation RP.PPC32))
il instr = i instr DLN.:| []

annotateInstrWith :: (forall x. D.Operand x -> D.Annotated b D.Operand x) -> RP.Instruction RP.OnlyEncoding a -> RP.Instruction RP.OnlyEncoding b
annotateInstrWith f instr = RP.fromAnnotatedInst RP.PPCRepr newDInstr
  where
    mapInstr (D.Instruction opc operands) = D.Instruction (coerce opc) (FC.fmapFC f operands)
    newDInstr = mapInstr (RP.toInst instr)


gpr :: Word8 -> D.Operand "Gprc"
gpr = D.Gprc . D.GPR

gpr_nor0 :: Word8 -> D.Operand "Gprc_nor0"
gpr_nor0 = D.Gprc_nor0 . D.GPR

regOffset :: Word8 -> Int16 -> D.Operand "Memri"
regOffset reg offset = D.Memri (D.MemRI (Just (D.GPR reg)) offset)

loadImm32 :: Word8 -> Word32 -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
loadImm32 reg imm =
  i (D.Instruction D.LIS (gpr reg :< D.S17imm (fromIntegral topBits) :< Nil)) DLN.:|
  [ i (D.Instruction D.ORI (gpr reg :< D.U16imm (fromIntegral lowBits) :< gpr reg :< Nil)) ]
  where
    topBits = imm `B.shiftR` 16
    lowBits = imm B..&. 0xFFFF

loadConcreteAddress :: Word8 -> R.ConcreteAddress RP.PPC32 -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
loadConcreteAddress reg addr = loadImm32 reg (fromIntegral (R.absoluteAddress addr))

withCallerSaveRegisters
  :: DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
  -> DLN.NonEmpty (R.Instruction RP.PPC32 RP.OnlyEncoding (R.Relocation RP.PPC32))
withCallerSaveRegisters instrs =
  makeSpace DLN.:| (pushRegisters ++ DLN.toList instrs ++ popRegisters ++ [unmakeSpace])
  where
    callerSaveGprs = 0:[3..12]
    totalRegs = length callerSaveGprs + 3 -- CR + LR + R1 itself
    storageSize = fromIntegral $ (totalRegs + (totalRegs `mod` 2)) * 4
    makeSpace = i $ D.Instruction D.STWU (regOffset 1 (-storageSize) :< gpr 1 :< Nil)
    unmakeSpace = i $ D.Instruction D.ADDI (gpr 1 :< D.S16imm storageSize :< gpr_nor0 1 :< Nil)
    crMask = 0xFFFFFFFF -- FIXME: More correct mask ?
    pushRegisters = map i $
      zipWith (
        \r off -> D.Instruction D.STW (regOffset 1 off :< gpr r :< Nil)
      ) callerSaveGprs [4, 8 ..] ++ [
        D.Instruction D.MFCR (gpr 7 :< Nil),
        D.Instruction D.STW (regOffset 1 (fromIntegral (length callerSaveGprs * 4 + 4)) :< gpr 7 :< Nil),
        D.Instruction D.MFLR (gpr 7 :< Nil),
        D.Instruction D.STW (regOffset 1 (fromIntegral (length callerSaveGprs * 4 + 8)) :< gpr 7 :< Nil)
     ]
    popRegisters = map i $
      [
        D.Instruction D.LWZ (gpr 7 :< regOffset 1 (fromIntegral (length callerSaveGprs * 4 + 4)) :< Nil),
        D.Instruction D.MTCRF (gpr 7 :< D.I32imm crMask :< Nil),
        D.Instruction D.LWZ (gpr 7 :< regOffset 1 (fromIntegral (length callerSaveGprs * 4 + 8)) :< Nil),
        D.Instruction D.MTLR (gpr 7 :< Nil)
      ] ++ zipWith (
        \r off -> D.Instruction D.LWZ (gpr r :< regOffset 1 off :< Nil)
      ) callerSaveGprs [4, 8 ..]
