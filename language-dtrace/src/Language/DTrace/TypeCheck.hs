{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Language.DTrace.TypeCheck (
    typeCheck
  , TypeErrorMessage(..)
  , TypeError(..)
  , SU.Builtin(..)
  , ppTypeError
  , ppTypeErrorMessage
  ) where

import           Control.Applicative ( (<|>) )
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.BitVector.Sized as DBS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Numeric.Natural ( Natural )
import qualified Prettyprinter as PP
import           Unsafe.Coerce ( unsafeCoerce )

import qualified Language.DTrace.LexerWrapper as LDL
import qualified Language.DTrace.Syntax.Typed as ST
import qualified Language.DTrace.Syntax.Untyped as SU
import qualified Language.DTrace.Token as DT


data TypeErrorMessage where
  IndeterminateInitializerType :: SU.Expr -> TypeErrorMessage
  InvalidAssignmentLHS :: LDL.Located SU.Expr -> TypeErrorMessage
  SignedLiteralOutOfRange :: PN.NatRepr n -> Natural -> TypeErrorMessage
  UnsignedLiteralOutOfRange :: PN.NatRepr n -> Natural -> TypeErrorMessage
  TypeMismatchOnAssignment :: ST.Repr tp1 -> ST.Repr tp2 -> TypeErrorMessage
  -- Expression, expected type, actual type
  ArgumentTypeMismatch :: LDL.Located SU.Expr -> ST.Repr tp1 -> ST.Repr tp2 -> TypeErrorMessage
  BinaryOperatorTypeMismatch :: String -> ST.Repr tp1 -> ST.Repr tp2 -> TypeErrorMessage
  InvalidOperandTypeForOperator :: String -> ST.Repr tp -> TypeErrorMessage

deriving instance Show TypeErrorMessage

instance PP.Pretty TypeErrorMessage where
  pretty = ppTypeErrorMessage

ppTypeErrorMessage :: TypeErrorMessage -> PP.Doc ann
ppTypeErrorMessage tag =
  case tag of
    IndeterminateInitializerType e -> PP.pretty "Could not determine the initializer type for the expression " <> PP.viaShow e
    InvalidAssignmentLHS e -> PP.pretty "Expression is not a valid target for an assignment: " <> PP.viaShow e
    SignedLiteralOutOfRange nr n ->
      PP.pretty "Signed literal " <> PP.pretty n <> PP.pretty " is too large for type of width " <> PP.viaShow (PN.natValue nr)
    UnsignedLiteralOutOfRange nr n ->
      PP.pretty "Unsigned literal " <> PP.pretty n <> PP.pretty " is too large for type of width " <> PP.viaShow (PN.natValue nr)
    TypeMismatchOnAssignment rhs varTy ->
      PP.pretty "Invalid assignment of expression with type " <> PP.viaShow rhs <> PP.pretty " to location with type " <> PP.viaShow varTy
    ArgumentTypeMismatch expr expected actual ->
      PP.pretty "Argument type mismatch for " <> PP.viaShow expr <> PP.pretty "; expected " <> PP.viaShow expected <> PP.pretty " but got " <>
        PP.viaShow actual
    BinaryOperatorTypeMismatch op lhs rhs ->
      PP.pretty "Invalid operands to operator " <> PP.pretty op <> PP.pretty ": " <> PP.viaShow lhs <> PP.pretty " and " <> PP.viaShow rhs
    InvalidOperandTypeForOperator op tty ->
      PP.pretty "Invalid operand type(s) for operator " <> PP.pretty op <> PP.pretty ": " <> PP.viaShow tty

data TypeError where
  TypeError :: LDL.SourceRange -> TypeErrorMessage -> TypeError

deriving instance Show TypeError

instance PP.Pretty TypeError where
  pretty = ppTypeError

ppTypeError :: TypeError -> PP.Doc ann
ppTypeError (TypeError range tag) =
  PP.pretty range <> PP.pretty ": " <> PP.pretty tag

typeError :: (LDL.HasRange a) => a -> TypeErrorMessage -> TypeError
typeError loc = TypeError (LDL.range loc)

recordError :: (LDL.HasRange a) => a -> TypeErrorMessage -> TC ()
recordError loc msg = RWS.tell (Seq.singleton (typeError loc msg))

data PState globals locals =
  PState { globalVars :: !(Ctx.Assignment ST.GlobalVariable globals)
         -- ^ Definitions of all of the global variables accessed in the script
         , globalMap :: !(Map.Map T.Text (Some (Ctx.Index globals)))
         -- ^ A map to look up variables in 'globalVars' by name
         , translatedProbes :: Seq.Seq (ST.Probe globals)
         -- ^ All of the probes that have been translated so far
         , localVars :: !(Ctx.Assignment ST.LocalVariable locals)
         -- ^ The locals for the *current* probe being translated
         , localMap :: !(Map.Map T.Text (Some (Ctx.Index locals)))
         -- ^ A map to look up variables in 'localVars' by name
         , probeStmts :: !(Seq.Seq (ST.Stmt globals locals))
         -- ^ The statements translated so far for the current probe
         --
         -- Note that these are in a-normal form (i.e., simple three address
         -- code without nested expressions)
         --
         -- FIXME: This will need to turn into a block structure eventually,
         -- unless side effects are acceptable in e.g. logical expressions and
         -- ternary operations
         , tempCounter :: Natural
         -- ^ A source of unique identifiers for local variables
         --
         -- Note that these are only used for printing names; indexing is done
         -- via the 'Ctx.Index' type.
         }

data State where
  State :: !(PState globals locals) -> State

emptyState :: State
emptyState = State pstate
  where
    pstate = PState { globalVars = Ctx.empty
                    , globalMap = Map.empty
                    , translatedProbes = mempty
                    , localVars = Ctx.empty
                    , localMap = Map.empty
                    , probeStmts = mempty
                    , tempCounter = 0
                    }

newtype TC a = TC { unTC :: RWS.RWS () (Seq.Seq TypeError) State a }
  deriving ( Functor
           , Applicative
           , Monad
           , RWS.MonadReader ()
           , RWS.MonadWriter (Seq.Seq TypeError)
           , RWS.MonadState State
           )



-- | Convert a Base Syntax type into a type repr from the Typed Syntax
typeRepr :: SU.Type -> Some ST.Repr
typeRepr ty =
  case ty of
    SU.CharTy -> Some (ST.BVRepr (PN.knownNat @8))
    SU.ShortTy -> Some (ST.BVRepr (PN.knownNat @16))
    SU.IntTy -> Some (ST.BVRepr (PN.knownNat @32))
    SU.SignedTy -> Some (ST.BVRepr (PN.knownNat @32))
    SU.UnsignedTy -> Some (ST.BVRepr (PN.knownNat @32))
    SU.LongTy -> Some (ST.BVRepr (PN.knownNat @64))
    SU.ULongTy -> Some (ST.BVRepr (PN.knownNat @64))
    SU.LongLongTy -> Some (ST.BVRepr (PN.knownNat @64))
    SU.ULongLongTy -> Some (ST.BVRepr (PN.knownNat @64))
    SU.FloatTy -> Some (ST.FloatRepr ST.SinglePrecRepr)
    SU.DoubleTy -> Some (ST.FloatRepr ST.DoublePrecRepr)
    SU.StringTy -> Some ST.StringRepr

-- | Attempt to compute a type repr for an expression
--
-- This could fail, at which point we would need to report an invalid
-- initializer that creates an indeterminate type.
typeOfExpr :: SU.Expr -> Maybe (Some ST.Repr)
typeOfExpr e =
  case SU.exprApp e of
    SU.This -> Nothing
    SU.Self -> Nothing
    SU.LitInt {} -> Just (Some (ST.BVRepr (PN.knownNat @32)))
    SU.LitString {} -> Just (Some ST.StringRepr)
    SU.LitDouble {} -> Just (Some (ST.FloatRepr ST.DoublePrecRepr))
    SU.LitFloat {} -> Just (Some (ST.FloatRepr ST.SinglePrecRepr))
    SU.LitLong {} -> Just (Some (ST.BVRepr (PN.knownNat @64)))
    SU.LitULong {} -> Just (Some (ST.BVRepr (PN.knownNat @64)))
    SU.LitLongLong {} -> Just (Some (ST.BVRepr (PN.knownNat @64)))
    SU.LitULongLong {} -> Just (Some (ST.BVRepr (PN.knownNat @64)))
    SU.Cast (LDL.Located _loc t) _ -> Just (typeRepr t)
    SU.Ternary _ e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Assign _lhs rhs ->
      -- To figure out the type of an assignment, we inspect the value being
      -- assigned
      typeOfExpr (LDL.value rhs)

    SU.Not e' -> typeOfExpr (LDL.value e')
    SU.And e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Or e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Xor e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)

    SU.Eq e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Ne e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Lt e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Le e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Ge e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Gt e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)

    SU.Neg e' -> typeOfExpr (LDL.value e')
    SU.Add e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Sub e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Div e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Mul e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.Mod e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)

    SU.BVNeg e' -> typeOfExpr (LDL.value e')
    SU.BVAnd e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.BVOr e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.BVXor e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.BVShl e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.BVLshr e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)
    SU.BVAshr e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)

typeOfBuiltin :: SU.Builtin -> Some ST.Repr
typeOfBuiltin SU.Timestamp = Some (ST.BVRepr n64)
typeOfBuiltin SU.UCaller = Some (ST.BVRepr n64) -- FIXME: I don't think this can always be right

-- | Return true if the given variable name is defined in the global state
--
-- This is pure to support use in pattern guards
undefinedVar :: PState globals locals -> T.Text -> Bool
undefinedVar s name = not (Map.member name (globalMap s))

resetLocals :: State -> State
resetLocals (State pstate) =
  State PState { globalVars = globalVars pstate
               , globalMap = globalMap pstate
               , translatedProbes = translatedProbes pstate
               , localVars = Ctx.empty
               , localMap = Map.empty
               , probeStmts = mempty
               , tempCounter = tempCounter pstate
               }

-- | Records a newly-translated probe and reset the local translation state
recordProbe :: ST.Probe globals -> PState globals locals -> PState globals Ctx.EmptyCtx
recordProbe p pstate =
  PState { globalVars = globalVars pstate
         , globalMap = globalMap pstate
         , translatedProbes = translatedProbes pstate Seq.|> p
         , localVars = Ctx.empty
         , localMap = Map.empty
         , probeStmts = mempty
         , tempCounter = tempCounter pstate
         }

addGlobal :: T.Text -> ST.Repr tp -> State -> State
addGlobal varName rep (State pstate) =
  State PState { globalVars = xvars
               , globalMap = xmap
               , translatedProbes = unsafeCoerce (translatedProbes pstate)
               , localVars = localVars pstate
               , localMap = localMap pstate
               , probeStmts = unsafeCoerce (probeStmts pstate)
               , tempCounter = tempCounter pstate
               }
  where
    var = ST.GlobalVariable rep varName
    xvars = globalVars pstate `Ctx.extend` var
    idx = Ctx.lastIndex (Ctx.size xvars)
    xmap = Map.insert varName (Some idx) (unsafeCoerce (globalMap pstate))

-- | Allocate globals that have explicit declarations (outside of any probe)
--
-- These semantically come first, establishing global variable types used in all
-- probes. They are used to force a global to have a specific type (in case
-- different probes initialize them with different types)
allocateExplicitGlobals :: SU.TopLevel -> TC ()
allocateExplicitGlobals tu =
  case tu of
    SU.TopProbe {} -> return ()
    SU.TopDecl ty name -> do
      case typeRepr (LDL.value ty) of
        Some rep -> RWS.modify' (addGlobal name rep)

-- | Allocate globals for all of the implicit declarations (established by the
-- first assignment)
--
-- DTrace allocates a fresh variable for the first assignment to a global.  This
-- function traverses the probes in source order and adds a global variable for
-- each assignment lhs.
--
-- NOTE: The real DTrace implementations perform the allocation based on the
-- first dynamic assignment. This implementation cannot do that because it is
-- based on static analysis.  The type of a variable in this code is based on
-- the type of the first assignment in probe definition order (and subject to
-- precedence in cases where multiple assignments to the same variable occur in
-- the same statement).
allocateImplicitGlobals :: SU.TopLevel -> TC ()
allocateImplicitGlobals tu =
  case tu of
    SU.TopDecl {} -> return ()
    SU.TopProbe lp -> mapM_ allocateProbeImplicitGlobals (SU.probeBody (LDL.value lp))
  where
    allocateProbeImplicitGlobals ls =
      case LDL.value ls of
        SU.DeclStmt {} -> return ()
        SU.ExprStmt le@(LDL.value -> SU.Expr app) -> do
          _ <- allocateGlobalLValues le
          _ <- SU.traverseExpr allocateGlobalLValues app
          return ()
    allocateGlobalLValues :: LDL.Located SU.Expr -> TC (LDL.Located SU.Expr)
    allocateGlobalLValues (LDL.Located range (SU.Expr app)) = do
      State s0 <- RWS.get
      case app of
        SU.Assign (LDL.Located _ (SU.Expr (SU.VarRef varName))) rhs
          | undefinedVar s0 varName ->
            case typeOfExpr (LDL.value rhs) of
              Just (Some tp) -> RWS.modify' (addGlobal varName tp)
              Nothing -> recordError rhs (IndeterminateInitializerType (LDL.value rhs))
        _ -> return ()
      app' <- SU.traverseExpr allocateGlobalLValues app
      return (LDL.Located range (SU.Expr app'))

inRangeSigned :: PN.NatRepr n -> Natural -> Bool
inRangeSigned nr n = n <= 2 ^ (PN.natValue nr - 1)

inRangeUnsigned :: PN.NatRepr n -> Natural -> Bool
inRangeUnsigned nr n = n <= 2 ^ PN.natValue nr

n32 :: PN.NatRepr 32
n32 = PN.knownNat @32

n64 :: PN.NatRepr 64
n64 = PN.knownNat @64


withFreshLocal :: forall globals locals tp a
                . PState globals locals
               -> ST.Repr tp
               -> (PState globals (locals Ctx.::> tp) -> Ctx.Index (locals Ctx.::> tp) tp -> a)
               -> a
withFreshLocal pstate0 rep k =
  k pstate1 (Ctx.lastIndex (Ctx.size newLocals))
  where
    varId = tempCounter pstate0
    newLocals = Ctx.extend (localVars pstate0) (ST.TemporaryVariable rep varId)
    pstate1 :: PState globals (locals Ctx.::> tp)
    pstate1 = PState { globalVars = globalVars pstate0
                     , globalMap = globalMap pstate0
                     , translatedProbes = translatedProbes pstate0
                     , probeStmts = unsafeCoerce (probeStmts pstate0)
                     , localMap = unsafeCoerce (localMap pstate0)
                     , localVars = newLocals
                     , tempCounter = varId + 1
                     }

setReg :: PState globals locals
       -> Ctx.Index locals tp
       -> ST.App (ST.Reg globals locals) tp
       -> PState globals locals
setReg pstate idx app =
  pstate { probeStmts = probeStmts pstate Seq.|> ST.SetReg idx (ST.Expr app)
         }


writeGlobal :: PState globals locals
            -> Ctx.Index globals tp
            -> ST.Reg globals locals tp
            -> PState globals locals
writeGlobal pstate dst src =
  pstate { probeStmts = probeStmts pstate Seq.|> ST.WriteGlobal dst src
         }

writeVoidStmt :: PState globals locals
              -> ST.App (ST.Reg globals locals) ST.VoidType
              -> PState globals locals
writeVoidStmt pstate call =
  pstate { probeStmts = probeStmts pstate Seq.|> ST.VoidStmt (ST.Expr call)
         }

-- | Translate a single untyped expression into a typed expression
--
-- Untyped expressions can have arbitrarily deep nesting structure, while the
-- typed AST is in A-normal form (i.e., three address code).  Thus, translating
-- expressions requires flattening them (which will add primitive statements to
-- the typechecker context under 'probeStmts').
--
-- Design 1: Mix local variables and temporaries
--
-- Design 2: Have a separate pool of named locals (like the global pool) and temporary bindings
--
-- It seems like an easy approach would be to bind each expression to a fresh
-- local variable.  This would admit a uniform representation.  This is Design 1.
--
-- The major challenge (in terms of types) is that 'translateExpr' needs to
-- extend the set of locals, but the locals type variable is:
--
-- 1. Quantified away here
--
-- 2. Inherently part of the temporary variable reference
--
-- This probably means that 'translateExpr' will not be in 'TC'; instead, it
-- will need to be CPSed to expose the type parameters of 'PState' to allow for
-- this growth.  Then the state will be quantified away by the caller.
translateExpr :: PState globals locals
              -> LDL.Located SU.Expr
              -> (forall loc . (LDL.HasRange loc) => loc -> TypeErrorMessage -> a)
              -> (forall locals' tp . PState globals locals' -> ST.Reg globals locals' tp -> a)
              -> a
translateExpr s0 ex0@(LDL.Located _ (SU.Expr app)) onError k =
  case app of
    SU.VarRef varName
      | Just (Some globalIdx) <- Map.lookup varName (globalMap s0) ->
        k s0 (ST.GlobalVar globalIdx)
    SU.BuiltinVarRef builtin
      | Some ty <- typeOfBuiltin builtin ->
        k s0 (ST.BuiltinVar builtin ty)
    SU.LitInt fmt n
      | not (inRangeSigned n32 n) -> onError ex0 (SignedLiteralOutOfRange n32 n)
      | otherwise -> withFreshLocal s0 (ST.BVRepr n32) $ \s1 idx ->
          let s2 = setReg s1 idx (ST.LitInt fmt n32 (DBS.mkBV n32 (toInteger n)))
          in k s2 (ST.LocalReg idx)
    SU.LitLong n
      | not (inRangeSigned n64 n) -> onError ex0 (SignedLiteralOutOfRange n64 n)
      | otherwise -> withFreshLocal s0 (ST.BVRepr n64) $ \s1 idx ->
          let s2 = setReg s1 idx (ST.LitInt DT.Decimal n64 (DBS.mkBV n64 (toInteger n)))
          in k s2 (ST.LocalReg idx)
    SU.LitULong n
      | not (inRangeUnsigned n64 n) -> onError ex0 (UnsignedLiteralOutOfRange n64 n)
      | otherwise -> withFreshLocal s0 (ST.BVRepr n64) $ \s1 idx ->
          let s2 = setReg s1 idx (ST.LitInt DT.Decimal n64 (DBS.mkBV n64 (toInteger n)))
          in k s2 (ST.LocalReg idx)
    SU.LitLongLong n
      | not (inRangeSigned n64 n) -> onError ex0 (SignedLiteralOutOfRange n64 n)
      | otherwise -> withFreshLocal s0 (ST.BVRepr n64) $ \s1 idx ->
          let s2 = setReg s1 idx (ST.LitInt DT.Decimal n64 (DBS.mkBV n64 (toInteger n)))
          in k s2 (ST.LocalReg idx)
    SU.LitULongLong n
      | not (inRangeUnsigned n64 n) -> onError ex0 (UnsignedLiteralOutOfRange n64 n)
      | otherwise -> withFreshLocal s0 (ST.BVRepr n64) $ \s1 idx ->
          let s2 = setReg s1 idx (ST.LitInt DT.Decimal n64 (DBS.mkBV n64 (toInteger n)))
          in k s2 (ST.LocalReg idx)
    SU.LitDouble t d -> withFreshLocal s0 (ST.FloatRepr ST.DoublePrecRepr) $ \s1 idx ->
      let s2 = setReg s1 idx (ST.LitFloat ST.DoublePrecRepr t d)
      in k s2 (ST.LocalReg idx)
    SU.LitFloat t d -> withFreshLocal s0 (ST.FloatRepr ST.SinglePrecRepr) $ \s1 idx ->
      let s2 = setReg s1 idx (ST.LitFloat ST.SinglePrecRepr t (realToFrac d))
      in k s2 (ST.LocalReg idx)
    SU.Assign (LDL.Located _ (SU.Expr (SU.VarRef varName))) rhs ->
      translateExpr s0 rhs onError $ \s1 rhsIdx -> do
        case Map.lookup varName (globalMap s0) of
          Nothing -> error ("Panic: No variable found for global: " ++ show varName)
          Just (Some globalIdx)
            | Just PC.Refl <- PC.testEquality (regType s1 rhsIdx) (ST.globalVarRepr (globalVars s1 Ctx.! globalIdx)) ->
              let s2 = writeGlobal s1 globalIdx rhsIdx
              in k s2 rhsIdx
            | otherwise -> onError ex0 (TypeMismatchOnAssignment (regType s1 rhsIdx) (ST.globalVarRepr (globalVars s1 Ctx.! globalIdx)))
    SU.Assign lhs _ -> onError ex0 (InvalidAssignmentLHS lhs)
    SU.Add lhs rhs -> binaryArith s0 ex0 lhs rhs onError k "Add" ST.BVAdd ST.FAdd
    SU.Sub lhs rhs -> binaryArith s0 ex0 lhs rhs onError k "Sub" ST.BVSub ST.FSub
    SU.Mul lhs rhs -> binaryArith s0 ex0 lhs rhs onError k "Mul" ST.BVMul ST.FMul

    -- "send" action: void send(u32)
    SU.Call name [n]
      | name == T.pack "send" ->
          translateExpr s0 n onError $ \s1 nIdx ->
              let ty = ST.BVRepr n32
              in case PC.testEquality (regType s1 nIdx) ty of
                  Nothing -> onError ex0 (ArgumentTypeMismatch n ty (regType s1 nIdx))
                  Just PC.Refl -> do
                      let s2 = writeVoidStmt s1 (ST.Call ST.VoidRepr name $ Ctx.singleton nIdx)
                      k s2 ST.VoidReg

    _ -> error ("Panic, unhandled expression: " ++ show ex0)

binaryArith :: PState globals locals
            -> LDL.Located SU.Expr
            -> LDL.Located SU.Expr
            -> LDL.Located SU.Expr
            -> (forall loc . (LDL.HasRange loc) => loc -> TypeErrorMessage -> a)
            -> (forall locals' tp . PState globals locals' -> ST.Reg globals locals' tp -> a)
            -> String
            -> (forall locals2 n . PN.NatRepr n -> ST.Reg globals locals2 (ST.BVType n) -> ST.Reg globals locals2 (ST.BVType n) -> ST.App (ST.Reg globals locals2) (ST.BVType n))
            -> (forall locals2 p . ST.FloatPrecRepr p -> ST.Reg globals locals2 (ST.FloatType p) -> ST.Reg globals locals2 (ST.FloatType p) -> ST.App (ST.Reg globals locals2) (ST.FloatType p))
            -> a
binaryArith s0 ex0 lhs rhs onError k opName bvOp fpOp =
  translateExpr s0 rhs onError $ \s1 rhsIdx -> do
    translateExpr s1 lhs onError $ \s2 lhsIdx -> do
      case PC.testEquality (regType s2 lhsIdx) (regType s1 rhsIdx) of
        Nothing -> onError ex0 (BinaryOperatorTypeMismatch opName (regType s2 lhsIdx) (regType s1 rhsIdx))
        Just PC.Refl ->
          withFreshLocal s2 (regType s2 lhsIdx) $ \s3 idx -> do
            case regType s2 lhsIdx of
              ST.BoolRepr -> onError ex0 (InvalidOperandTypeForOperator opName ST.BoolRepr)
              ST.StringRepr -> onError ex0 (InvalidOperandTypeForOperator opName ST.StringRepr)
              ST.VoidRepr -> onError ex0 (InvalidOperandTypeForOperator opName ST.VoidRepr)
              ST.BVRepr n ->
                let s4 = setReg s3 idx (bvOp n (extendReg lhsIdx) (unsafeCoerce rhsIdx))
                in k s4 (ST.LocalReg idx)
              ST.FloatRepr p ->
                let s4 = setReg s3 idx (fpOp p (extendReg lhsIdx) (unsafeCoerce rhsIdx))
                in k s4 (ST.LocalReg idx)

extendReg :: (Ctx.KnownDiff l r) => ST.Reg globals l tp -> ST.Reg globals r tp
extendReg r =
  case r of
    ST.LocalReg localIdx -> ST.LocalReg (Ctx.extendIndex localIdx)
    ST.GlobalVar globalIdx -> ST.GlobalVar globalIdx
    ST.BuiltinVar builtin t -> ST.BuiltinVar builtin t
    ST.VoidReg -> ST.VoidReg

regType :: PState globals locals -> ST.Reg globals locals tp -> ST.Repr tp
regType s r =
  case r of
    ST.LocalReg localIdx -> ST.localVarRepr (localVars s Ctx.! localIdx)
    ST.GlobalVar globalIdx -> ST.globalVarRepr (globalVars s Ctx.! globalIdx)
    ST.BuiltinVar _ t -> t
    ST.VoidReg -> ST.VoidRepr

translateStatement :: [LDL.Located SU.Stmt] -> TC () -> TC () -> TC ()
translateStatement [] _exitEarly k = k
translateStatement (stmt:stmts) exitEarly k =
  case LDL.value stmt of
    SU.DeclStmt {} -> return ()
    SU.ExprStmt ex -> do
      State s0 <- RWS.get
      let onError :: (LDL.HasRange a) => a -> TypeErrorMessage -> TC ()
          onError a msg = recordError a msg >> exitEarly
      translateExpr s0 ex onError $ \s1 _idx -> do
        RWS.put (State s1)
        translateStatement stmts exitEarly k

checkProbe :: SU.TopLevel -> TC ()
checkProbe tu =
  case tu of
    SU.TopDecl {} -> return ()
    SU.TopProbe p -> do
      RWS.modify' resetLocals

      let cleanupLocals = RWS.modify' resetLocals
      translateStatement (SU.probeBody (LDL.value p)) cleanupLocals $ do
        State s <- RWS.get
        let true = ST.Expr (ST.LitBool True)
        let patterns = fmap LDL.value (SU.probePatterns (LDL.value p))
        let translatedProbe = ST.Probe patterns true (localVars s) (F.toList (probeStmts s))
        RWS.put (State (recordProbe translatedProbe s))

doTypeCheck :: [SU.TopLevel] -> TC ()
doTypeCheck topLevels = do
  mapM_ allocateExplicitGlobals topLevels
  mapM_ allocateImplicitGlobals topLevels
  mapM_ checkProbe topLevels

-- | Type check definitions from the untyped AST into a typed AST with safe variable references (or errors)
typeCheck :: [LDL.Located SU.TopLevel] -> Either [TypeError] ST.Probes
typeCheck (fmap LDL.value -> topLevels) =
  case RWS.execRWS (unTC (doTypeCheck topLevels)) () emptyState of
    (State finalPState, errs)
      | Seq.null errs -> Right (ST.Probes (globalVars finalPState) (globalMap finalPState) (F.toList (translatedProbes finalPState)))
      | otherwise -> Left (F.toList errs)
