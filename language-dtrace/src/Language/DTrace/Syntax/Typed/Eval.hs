{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Language.DTrace.Syntax.Typed.Eval (
    RegEntry
  , EvaluatorState
  , initialEvaluatorState
  , evalProbe
  ) where

import qualified Control.Lens as L
import qualified Data.BitVector.Sized as DBS
import qualified Data.Foldable as F
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Text as T

import qualified Language.DTrace.Syntax.Typed as ST

data family RegEntry (a :: ST.Type)
data instance RegEntry ST.BoolType = RegBool !Bool
data instance RegEntry (ST.BVType n) = RegBV !(DBS.BV n)
data instance RegEntry (ST.FloatType ST.SinglePrec) = RegFloat !Float
data instance RegEntry (ST.FloatType ST.DoublePrec) = RegDouble !Double
data instance RegEntry ST.StringType = RegString !T.Text

data EvaluatorState globals where
  EvaluatorState :: !(Ctx.Assignment RegEntry globals) -> EvaluatorState globals

-- | Zero-initialize all state entries
initialEvaluatorState :: forall tps . Ctx.Assignment ST.Variable tps -> EvaluatorState tps
initialEvaluatorState vars = EvaluatorState (Ctx.generate (Ctx.size vars) initialVariableValue)
  where
    initialVariableValue :: Ctx.Index tps tp -> RegEntry tp
    initialVariableValue idx =
      case ST.varRepr (vars Ctx.! idx) of
        ST.BoolRepr -> RegBool False
        ST.BVRepr n -> RegBV (DBS.mkBV n 0)
        ST.StringRepr -> RegString mempty
        ST.FloatRepr ST.SinglePrecRepr -> RegFloat 0
        ST.FloatRepr ST.DoublePrecRepr -> RegDouble 0

regVal :: Ctx.Assignment RegEntry globals
       -> Ctx.Assignment RegEntry locals
       -> ST.Reg globals locals tp
       -> RegEntry tp
regVal globals locals reg =
  case reg of
    ST.LocalReg idx -> locals Ctx.! idx
    ST.GlobalVar idx -> globals Ctx.! idx

evalApp :: Ctx.Assignment RegEntry globals
        -> Ctx.Assignment RegEntry locals
        -> ST.App (ST.Reg globals locals) tp
        -> RegEntry tp
evalApp globals locals app =
  case app of
    ST.LitInt _fmt _rep bv -> RegBV bv
    ST.LitString t -> RegString t
    ST.LitFloat ST.SinglePrecRepr _txt f -> RegFloat (realToFrac f)
    ST.LitFloat ST.DoublePrecRepr _txt d -> RegDouble d
    ST.LitBool b -> RegBool b

    ST.And e1 e2 ->
      case (regVal globals locals e1, regVal globals locals e2) of
        (RegBool b1, RegBool b2) -> RegBool (b1 && b2)
    ST.Or e1 e2 ->
      case (regVal globals locals e1, regVal globals locals e2) of
        (RegBool b1, RegBool b2) -> RegBool (b1 || b2)
    ST.Xor e1 e2 ->
      case (regVal globals locals e1, regVal globals locals e2) of
        (RegBool b1, RegBool b2) -> RegBool (b1 && not b2 || not b1 && b2)
    ST.Not e ->
      case regVal globals locals e of
        RegBool b -> RegBool (not b)

    ST.BVNeg nr e ->
      case regVal globals locals e of
        RegBV b -> RegBV (DBS.negate nr b)
    ST.BVAdd nr e1 e2 ->
      case (regVal globals locals e1, regVal globals locals e2) of
        (RegBV b1, RegBV b2) -> RegBV (DBS.add nr b1 b2)
    ST.BVSub nr e1 e2 ->
      case (regVal globals locals e1, regVal globals locals e2) of
        (RegBV b1, RegBV b2) -> RegBV (DBS.sub nr b1 b2)
    ST.BVMul nr e1 e2 ->
      case (regVal globals locals e1, regVal globals locals e2) of
        (RegBV b1, RegBV b2) -> RegBV (DBS.mul nr b1 b2)
    ST.BVAnd _ e1 e2 ->
      case (regVal globals locals e1, regVal globals locals e2) of
        (RegBV b1, RegBV b2) -> RegBV (b1 `DBS.and` b2)

evalStmt :: (EvaluatorState globals, EvaluatorState locals)
         -> ST.Stmt globals locals
         -> (EvaluatorState globals, EvaluatorState locals)
evalStmt (g0@(EvaluatorState globals), l0@(EvaluatorState locals)) stmt =
  case stmt of
    ST.SetReg idx (ST.Expr app) -> (g0, EvaluatorState (L.set (PC.ixF idx) (evalApp globals locals app) locals))
    ST.WriteGlobal globalIdx localIdx -> (EvaluatorState (L.set (PC.ixF globalIdx) (locals Ctx.! localIdx) globals), l0)

-- | Given a 'ST.Probe' and an initial state, evaluate the probe to compute the final state
--
-- FIXME: This does not account for the guard yet
evalProbe :: ST.Probe globals -> EvaluatorState globals -> EvaluatorState globals
evalProbe (ST.Probe _desc guard localVars stmts) env =
  fst (F.foldl' evalStmt (env, locals0) stmts)
  where
    locals0 = initialEvaluatorState localVars
