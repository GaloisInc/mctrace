{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Generate an LLVM 'IR.Module' for a set of DTrace probes
--
-- This module only performs the pure codegen step; clients must invoke the LLVM
-- code generators manually.  This split of responsibilities it to ensure that
-- the core library doesn't need to link directly against any C++ shared
-- libraries, which can be problematic.
module MCTrace.Codegen.LLVM (
    CompiledProbes(..)
  , compileProbesLLVM
  , withLLVMOptions
  ) where

import qualified Control.Exception as X
import           Control.Monad ( void, when )
import qualified Control.Monad.Reader as CMR
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Trans as CMT
import qualified Data.BitVector.Sized as DBS
import qualified Data.ElfEdit as DE
import qualified Data.Functor.Const as C
import qualified Data.Functor.Identity as I
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.String ( fromString )
import           Data.Word ( Word32 )
import qualified LLVM.AST as IR
import qualified LLVM.AST.AddrSpace as IRA
import qualified LLVM.AST.Constant as IRC
import qualified LLVM.AST.Float as IRF
import qualified LLVM.AST.Type as IRT
import qualified LLVM.CodeGenOpt as LLCGO
import qualified LLVM.CodeModel as LLC
import qualified LLVM.IRBuilder as IRB
import qualified LLVM.IRBuilder.Constant as IRBC
import qualified LLVM.Relocation as LLR
import qualified LLVM.Target as LLT
import qualified Language.DTrace.Syntax.Typed as ST
import qualified Language.DTrace.TypeCheck as TC

import qualified MCTrace.Builtins as MB
import qualified MCTrace.Exceptions as ME
import qualified MCTrace.Panic as MP
import qualified MCTrace.RuntimeAPI as RT

-- | The global translation environment, which records the offset from the
-- storage base pointer for each global variable
--
-- See Note [CodeGen Strategy] for details
data Env (globals :: Ctx.Ctx ST.Type) =
  Env { globalVarOffsets :: Ctx.Assignment (C.Const Word32) globals
      , globalVars :: Ctx.Assignment ST.GlobalVariable globals
      }

-- | The reader environment for the LLVM IR builder
newtype Builder globals a = Builder { unBuilder :: CMR.ReaderT (Env globals) (IRB.ModuleBuilderT I.Identity) a }
  deriving ( Functor
           , Monad
           , Applicative
           , CMR.MonadReader (Env globals)
           , IRB.MonadModuleBuilder
           )

-- | Convert the internal type representative to the corresponding LLVM IR type
reprToType :: ST.Repr tp -> IR.Type
reprToType r =
  case r of
    ST.BoolRepr -> IRT.IntegerType { IRT.typeBits = 1 }
    ST.BVRepr n -> IRT.IntegerType { IRT.typeBits = fromIntegral (PN.natValue n) }
    ST.FloatRepr ST.SinglePrecRepr ->
      IRT.FloatingPointType { IRT.floatingPointType = IRT.FloatFP }
    ST.FloatRepr ST.DoublePrecRepr ->
      IRT.FloatingPointType { IRT.floatingPointType = IRT.DoubleFP }
    ST.VoidRepr ->
      IRT.VoidType
    ST.StringRepr ->
      error "reprToType: StringRepr unsupported"

-- | Return the zero initializer for each internal base type
zeroInit :: ST.Repr tp -> IRC.Constant
zeroInit r =
  case r of
    ST.BoolRepr -> IRC.Int { IRC.integerBits = 1, IRC.integerValue = 0 }
    ST.BVRepr n -> IRC.Int { IRC.integerBits = fromIntegral (PN.natValue n), IRC.integerValue = 0 }
    ST.FloatRepr ST.SinglePrecRepr ->
      IRC.Float { IRC.floatValue = IRF.Single 0 }
    ST.FloatRepr ST.DoublePrecRepr ->
      IRC.Float { IRC.floatValue = IRF.Double 0 }
    ST.VoidRepr ->
      IRC.Null { IRC.constantType = IRT.VoidType }
    ST.StringRepr ->
      error "zeroInit: StringRepr unsupported"

stackAlignment :: Word32
stackAlignment = 8

allocateLocal :: Ctx.Index locals tp -> ST.LocalVariable tp -> IRB.IRBuilderT (Builder globals) (C.Const IR.Operand tp)
allocateLocal _idx var =
  C.Const <$> IRB.alloca (reprToType rep) (Just (IR.ConstantOperand (zeroInit rep))) stackAlignment
  where
    rep = ST.localVarRepr var

-- | Generate an LLVM 'IR.Operand' that is the address of the global variable
-- location indicated by the given 'Ctx.Index'
--
-- This is necessary because the storage is handled outside of the module and
-- passed to each probe as a parameter (see Note [CodeGen Strategy])
globalVarOperand :: GlobalStore
                 -> Ctx.Index globals tp
                 -> IRB.IRBuilderT (Builder globals) IR.Operand
globalVarOperand (GlobalStore storeOp) idx = do
  gvs <- CMT.lift (CMR.asks globalVars)
  globalOffsets <- CMT.lift (CMR.asks globalVarOffsets)
  let off = C.getConst (globalOffsets Ctx.! idx)
  -- The storage operand is a `char**`, so we need an initial dereference to get
  -- the `char*` that we can GEP
  ptr0 <- IRB.load storeOp 0
  ptr <- IRB.gep ptr0 [IRB.int32 (toInteger off)]
  let ptrType = IRT.PointerType (reprToType (ST.globalVarRepr (gvs Ctx.! idx))) (IRA.AddrSpace 0)
  IRB.bitcast ptr ptrType

op :: GlobalStore
   -> ProbeSupportFunctions
   -> UCallerPointer
   -> Ctx.Assignment (C.Const IR.Operand) locals
   -> ST.Reg globals locals tp
   -> IRB.IRBuilderT (Builder globals) IR.Operand
op globals (ProbeSupportFunctions supportFnsOperand) (UCallerPointer uCaller) locals reg =
  case reg of
    ST.LocalReg idx -> IRB.load (C.getConst (locals Ctx.! idx)) stackAlignment
    ST.GlobalVar idx -> do
      gptr <- globalVarOperand globals idx
      IRB.load gptr 0
    ST.BuiltinVar builtin _ -> do
      -- NOTE: Simplified. Need fix for more complex built-ins
      let MB.BuiltinVarCompiler { MB.compile = provider } = MB.builtinVarCompilers Map.! builtin
      provider (MB.BuiltinVarCompilerArgs supportFnsOperand uCaller Nothing)
    ST.VoidReg ->
      error "op: VoidReg not supported"

regTypeRepr :: Ctx.Assignment ST.LocalVariable locals
            -> ST.Reg globals locals tp
            -> IRB.IRBuilderT (Builder globals) (ST.Repr tp)
regTypeRepr localVars reg =
  case reg of
    ST.LocalReg idx ->
      return $ ST.localVarRepr $ localVars Ctx.! idx
    ST.GlobalVar idx -> do
      gvs <- CMT.lift (CMR.asks globalVars)
      return $ ST.globalVarRepr $ gvs Ctx.! idx
    ST.BuiltinVar _ r ->
      return r
    ST.VoidReg ->
      return ST.VoidRepr

mapply2 :: (Monad m) => (t1 -> t2 -> m b) -> m t1 -> m t2 -> m b
mapply2 f o1 o2 = do
  o1' <- o1
  o2' <- o2
  f o1' o2'

compileExpr :: GlobalStore
            -> ProbeSupportFunctions
            -> UCallerPointer
            -> Ctx.Assignment ST.LocalVariable locals
            -> Ctx.Assignment (C.Const IR.Operand) locals
            -> ST.App (ST.Reg globals locals) tp
            -> IRB.IRBuilderT (Builder globals) IR.Operand
compileExpr globals supportFnsOperand uCallerPointer localVars localOperands app =
  case app of
    ST.LitInt _fmt rep bv ->
      return $
          IR.ConstantOperand IRC.Int { IRC.integerBits = fromIntegral (PN.natValue rep)
                                     , IRC.integerValue = DBS.asUnsigned bv
                                     }
    ST.BVAdd _ r1 r2 ->
        mapply2 IRB.add (op globals supportFnsOperand uCallerPointer localOperands r1)
                        (op globals supportFnsOperand uCallerPointer localOperands r2)
    ST.BVSub _ r1 r2 ->
        mapply2 IRB.sub (op globals supportFnsOperand uCallerPointer localOperands r1)
                        (op globals supportFnsOperand uCallerPointer localOperands r2)
    ST.BVMul _ r1 r2 ->
        mapply2 IRB.mul (op globals supportFnsOperand uCallerPointer localOperands r1)
                        (op globals supportFnsOperand uCallerPointer localOperands r2)
    ST.BVAnd _ r1 r2 ->
        mapply2 IRB.and (op globals supportFnsOperand uCallerPointer localOperands r1)
                        (op globals supportFnsOperand uCallerPointer localOperands r2)
    ST.BVOr _ r1 r2 ->
        mapply2 IRB.or (op globals supportFnsOperand uCallerPointer localOperands r1)
                       (op globals supportFnsOperand uCallerPointer localOperands r2)
    ST.BVXor _ r1 r2 ->
        mapply2 IRB.xor (op globals supportFnsOperand uCallerPointer localOperands r1)
                        (op globals supportFnsOperand uCallerPointer localOperands r2)
    ST.Call ST.VoidRepr "send" args -> do
        let sz = Ctx.sizeInt $ Ctx.size args
        when (sz /= 1) $
            error $ "BUG: compileExpr: send() call had arg list of unexpected length: " <> show sz
        Ctx.traverseWithIndex_ (compileSend globals supportFnsOperand uCallerPointer localVars localOperands) args
        return $ IR.ConstantOperand $ zeroInit ST.VoidRepr

    ST.Call (ST.BVRepr n) "copyint32" args
        | Just _ <- PN.testEquality n TC.n32 -> do
            let expectedArgsRepr = Ctx.singleton (ST.BVRepr TC.n32)
            argsRepr <- Ctx.traverseWithIndex (const $ regTypeRepr localVars) args
            case PN.testEquality argsRepr expectedArgsRepr of
                Nothing ->
                    MP.panic MP.LLVMCodegen "compileExpr"
                             ["copyint32() call had arg list of unexpected length"]
                Just PN.Refl -> do
                    ops <- Ctx.traverseAndCollect (compileCopyInt32 globals supportFnsOperand
                                                   uCallerPointer localVars localOperands) args
                    return $ ops !! 0

    _ -> error "compileExpr: got an unsupported expression"

compileCopyInt32 :: GlobalStore
                 -> ProbeSupportFunctions
                 -> UCallerPointer
                 -> Ctx.Assignment ST.LocalVariable locals
                 -> Ctx.Assignment (C.Const IR.Operand) locals
                 -> Ctx.Index tps tp
                 -> ST.Reg globals locals tp
                 -> IRB.IRBuilderT (Builder globals) [IR.Operand]
compileCopyInt32 globals supportFnsOperand uCallerPointer localVars localOperands _ arg = do
    let ty = ST.BVRepr TC.n32
    r <- regTypeRepr localVars arg
    case PN.testEquality ty r of
        Nothing ->
            MP.panic MP.LLVMCodegen "compileCopyInt32" ["got a copyint32() argument that isn't the right type"]
        Just PN.Refl -> do
            valOperand <- op globals supportFnsOperand uCallerPointer localOperands arg
            addrOperand <- IRB.inttoptr valOperand $ pointerType IRT.i32
            (:[]) <$> IRB.load addrOperand 0

compileSend :: GlobalStore
            -> ProbeSupportFunctions
            -> UCallerPointer
            -> Ctx.Assignment ST.LocalVariable locals
            -> Ctx.Assignment (C.Const IR.Operand) locals
            -> Ctx.Index tps tp
            -> ST.Reg globals locals tp
            -> IRB.IRBuilderT (Builder globals) ()
compileSend globals supportFnsOperand uCallerPointer localVars localOperands _ arg = do
    let ty = ST.BVRepr TC.n32
    r <- regTypeRepr localVars arg
    case PN.testEquality ty r of
        Nothing -> error "BUG: compileSend got a send() argument that isn't the right type"
        Just PN.Refl -> do
            valOperand <- op globals supportFnsOperand uCallerPointer localOperands arg
            sendStatement globals supportFnsOperand valOperand

compileStatement :: GlobalStore
                 -> ProbeSupportFunctions
                 -> UCallerPointer
                 -> Ctx.Assignment ST.LocalVariable locals
                 -> Ctx.Assignment (C.Const IR.Operand) locals
                 -> ST.Stmt globals locals
                 -> IRB.IRBuilderT (Builder globals) ()
compileStatement globals supportFnsOperand uCallerPointer localVars localOperands stmt =
  case stmt of
    ST.SetReg localIdx (ST.Expr app) -> do
      operand <- compileExpr globals supportFnsOperand uCallerPointer localVars localOperands app
      IRB.store (C.getConst (localOperands Ctx.! localIdx)) 0 operand
    ST.WriteGlobal globalIdx (ST.LocalReg localIdx) -> do
      dst <- globalVarOperand globals globalIdx
      val <- IRB.load (C.getConst (localOperands Ctx.! localIdx)) stackAlignment
      IRB.store dst 0 val
    ST.WriteGlobal globalIdx (ST.GlobalVar srcIdx) -> do
      src <- globalVarOperand globals srcIdx
      dst <- globalVarOperand globals globalIdx
      val <- IRB.load src stackAlignment
      IRB.store dst 0 val
    ST.WriteGlobal globalIdx (ST.BuiltinVar builtin _) -> do
      let MB.BuiltinVarCompiler { MB.compile = provider } = MB.builtinVarCompilers Map.! builtin
          (ProbeSupportFunctions supportFnsOp) = supportFnsOperand
          (UCallerPointer uCaller) = uCallerPointer
      dst <- globalVarOperand globals globalIdx
      val <- provider (MB.BuiltinVarCompilerArgs supportFnsOp uCaller Nothing)
      IRB.store dst 0 val
    ST.WriteGlobal {} ->
      error "compileStatement: got unsupported WriteGlobal"
    ST.VoidStmt (ST.Expr app) ->
      void $ compileExpr globals supportFnsOperand uCallerPointer localVars localOperands app

compileProbeBody :: GlobalStore
                 -> ProbeSupportFunctions
                 -> UCallerPointer
                 -> Ctx.Assignment ST.LocalVariable locals
                 -> [ST.Stmt globals locals]
                 -> IRB.IRBuilderT (Builder globals) ()
compileProbeBody globalOperands supportFnsOperand uCallerPointer localVars stmts = do
  localOperands <- Ctx.traverseWithIndex allocateLocal localVars
  mapM_ (compileStatement globalOperands supportFnsOperand uCallerPointer localVars localOperands) stmts

sendStatement :: GlobalStore
              -> ProbeSupportFunctions
              -> IR.Operand
              -> IRB.IRBuilderT (Builder globals) ()
sendStatement (GlobalStore globalStore) (ProbeSupportFunctions probeSupportFunctions) arg = do
  fnAddr <- IRB.gep probeSupportFunctions [IRB.int32 sendFnIndex]
  fn <- IRB.load fnAddr 0
  castedFn <- IRB.bitcast fn (pointerType sendFnType)
  gStore <- IRB.load globalStore 0
  castedGlobalStore <- IRB.bitcast gStore (pointerType IRT.i8)
  globalsSize <- fromIntegral <$> calcGlobalsSize
  let operands = [(arg, []), (castedGlobalStore, []), (IRBC.int32 globalsSize, [])]
  CMS.void $ IRB.call castedFn operands
  where
    sendFnIndex = fromIntegral $ RT.probeSupportFunctionIndexMap Map.! RT.Send
    sendFnType = IRT.FunctionType IRT.VoidType [IRT.i32, pointerType IRT.i8, IRT.i32] False
    calcGlobalsSize :: IRB.IRBuilderT (Builder globals) Word32
    calcGlobalsSize = do
      gvs <- CMT.lift (CMR.asks globalVars)
      let count = Ctx.sizeInt $ Ctx.size gvs
      return $ fromIntegral count * globalSlotSize


data GlobalStore = GlobalStore IR.Operand
data ProbeSupportFunctions = ProbeSupportFunctions IR.Operand
data UCallerPointer = UCallerPointer IR.Operand

pointerType :: IRT.Type -> IRT.Type
pointerType t = IRT.PointerType t (IRA.AddrSpace 0)

compileProbe :: (ST.Probe globals, String) -> Builder globals ()
compileProbe (ST.Probe _descs _guard localVars stmts, fname) = do
  let storage = (pointerType (pointerType (IRT.IntegerType 8)), IRB.ParameterName (fromString "storage"))
  let supportFns = (pointerType (pointerType dummyFunType), IRB.ParameterName (fromString "supportfns"))
  let uCaller = (pointerType (IRT.IntegerType 64), IRB.ParameterName (fromString "ucaller"))
  _ <- IRB.function (IR.mkName fname) [storage, supportFns, uCaller] IRT.VoidType $ \operands ->
    case operands of
      [storageOp, supportFnsOp, uCallerOp] ->
        compileProbeBody (GlobalStore storageOp) (ProbeSupportFunctions supportFnsOp) (UCallerPointer uCallerOp) localVars stmts
      _ -> MP.panic MP.LLVMCodegen "compileProbe" ["Impossible argument list"]
  return ()
  where
    dummyFunType = IRT.FunctionType (pointerType IRT.i64) [IRT.i64, IRT.i64] False


globalSlotSize :: Word32
globalSlotSize = 8

-- | Allocate locations for the global variables (offsets from the base of the
-- data store)
--
-- Each location is 8 byte aligned (the largest data item currently supported)
-- for simplicity.
allocateGlobal :: Ctx.Index globals tp
               -> ST.GlobalVariable tp
               -> CMS.State Word32 (C.Const Word32 tp)
allocateGlobal _idx _var = do
  off <- CMS.get
  CMS.modify' (+ globalSlotSize)
  return (C.Const off)

-- | The results of compiling a set of probes to LLVM
--
-- This captures the offsets assigned to each global, as well as the symbol
-- names assigned to each generated probe function.
data CompiledProbes where
  CompiledProbes :: Ctx.Assignment ST.GlobalVariable globals
                 -- ^ global variable definitions
                 -> Ctx.Assignment (C.Const Word32) globals
                 -- ^ global variable offsets
                 -> [(ST.Probe globals, String)]
                 -- ^ probe function symbol names
                 -> IR.Module
                 -- ^ the resulting module
                 -> CompiledProbes

probeName :: Int -> String
probeName idx = "probe" ++ show idx

compileProbesLLVM :: String -> ST.Probes -> CompiledProbes
compileProbesLLVM moduleName (ST.Probes globalVarDefs _globalMap probes) =
  CompiledProbes globalVarDefs varOffsets namedProbes llvmMod
  where
    namedProbes = [ (p, probeName idx)
                  | (idx, p) <- zip [0..] probes
                  ]
    llvmMod = I.runIdentity (IRB.buildModuleT name (CMR.runReaderT initialAct initialEnv))
    varOffsets = CMS.evalState (Ctx.traverseWithIndex allocateGlobal globalVarDefs) 0
    name = fromString moduleName
    initialAct = unBuilder (mapM_ compileProbe namedProbes)
    initialEnv = Env { globalVarOffsets = varOffsets
                     , globalVars = globalVarDefs
                     }

-- | Build an LLVM 'LLT.TargetMachine' based on a loaded ELF file
withLLVMOptions :: DE.SomeElf DE.ElfHeaderInfo
                -> (LLT.TargetMachine -> IO a)
                -> IO a
withLLVMOptions (DE.SomeElf ehi) k =
  case DE.headerMachine (DE.header ehi) of
    DE.EM_X86_64 -> do
      let triple = fromString "x86_64-unknown-linux-gnu"
      (t, _) <- LLT.lookupTarget Nothing triple
      let cpu = fromString ""
      let features = Map.empty
      LLT.withTargetOptions $ \targetOpts -> do
        LLT.withTargetMachine t triple cpu features targetOpts LLR.Default LLC.Default LLCGO.Aggressive k
    DE.EM_PPC -> do
      let triple = fromString "ppc32-unknown-linux"
      (t, _) <- LLT.lookupTarget Nothing triple
      let cpu = fromString ""
      let features = Map.empty
      LLT.withTargetOptions $ \targetOpts -> do
        LLT.withTargetMachine t triple cpu features targetOpts LLR.Default LLC.Default LLCGO.Aggressive k
    m ->
      X.throwIO (ME.UnsupportedArchitecture m)


{- Note [CodeGen Strategy]

One of the key challenges of code generation for instrumentation purposes is
variable storage (in a way that supports capturing computed trace values).  Just
storing the necessary values in global memory is not straightforward, as
capturing those values for analysis is difficult (it would at least require some
kind of hook to dump the values to persistent storage when the program exits,
which is difficult in the case of unclean exits).

One strategy is to memory map the necessary storage over a file and have probes
write directly to this mmapped storage.

From the perspective of this code generator, we do not want to have to worry
about this or make commitments to the underlying storage mechanism.  Instead,
each probe will take a pointer to the storage pool (whatever its form is) and
write to the appropriate offset into the pool for each variable.  The
`globalVars` field of the environment tracks all of the offsets.

NOTE: This does not account for the allocation of the required storage; users of
this code must arrange for that

-}
