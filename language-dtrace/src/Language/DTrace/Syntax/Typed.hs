{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Language.DTrace.Syntax.Typed (
    Type(..)
  , FloatType
  , SinglePrec
  , DoublePrec
  , BVType
  , BoolType
  , StringType
  , FloatPrecRepr(..)
  , Repr(..)
  , App(..)
  , Reg(..)
  , LocalVariable(..)
  , localVarRepr
  , GlobalVariable(..)
  , globalVarName
  , globalVarRepr
  , Expr(..)
  , Stmt(..)
  , Probe(..)
  , Probes(..)
  ) where

import qualified Data.BitVector.Sized as DBS
import qualified Data.Kind as DK
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map as Map
import           Data.Maybe ( isJust )
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.TH.GADT as PTH
import qualified Data.Text as T
import           GHC.TypeLits ( Nat )
import           Numeric.Natural ( Natural )

import qualified Language.DTrace.ProbeDescription as LDP
import qualified Language.DTrace.Token as DT

data FloatPrecision where
  SinglePrec :: FloatPrecision
  DoublePrec :: FloatPrecision

type SinglePrec = 'SinglePrec
type DoublePrec = 'DoublePrec

data FloatPrecRepr p where
  SinglePrecRepr :: FloatPrecRepr SinglePrec
  DoublePrecRepr :: FloatPrecRepr DoublePrec

deriving instance Show (FloatPrecRepr p)

data Type where
  BVType :: Nat -> Type
  BoolType :: Type
  FloatType :: FloatPrecision -> Type
  StringType :: Type
  VoidType :: Type

$(return [])

instance PC.TestEquality FloatPrecRepr where
  testEquality = $(PTH.structuralTypeEquality [t|FloatPrecRepr|] [])

type BVType = 'BVType
type BoolType = 'BoolType
type FloatType = 'FloatType
type StringType = 'StringType

data Repr tp where
  BVRepr :: PN.NatRepr n -> Repr (BVType n)
  BoolRepr :: Repr BoolType
  FloatRepr :: FloatPrecRepr p -> Repr (FloatType p)
  StringRepr :: Repr StringType

$(return [])

instance PC.TestEquality Repr where
  testEquality = $(PTH.structuralTypeEquality [t|Repr|]
                  [ (PTH.TypeApp (PTH.ConType [t|PN.NatRepr|]) PTH.AnyType, [|PC.testEquality|])
                  , (PTH.TypeApp (PTH.ConType [t|FloatPrecRepr|]) PTH.AnyType, [|PC.testEquality|])
                  ])

instance Eq (Repr tp) where
  r1 == r2 = isJust (PC.testEquality r1 r2)

deriving instance Show (Repr tp)

data Reg globals locals (tp :: Type) where
  LocalReg :: Ctx.Index locals tp -> Reg globals locals tp
  GlobalVar :: Ctx.Index globals tp -> Reg globals locals tp

newtype Expr globals locals (tp :: Type) = Expr { exprApp :: App (Reg globals locals) tp }

data App (f :: Type -> DK.Type) (tp :: Type) where
  This :: Repr tp -> App f tp
  Self :: Repr tp -> App f tp

  LitInt :: DT.NumericLiteralFormat -> PN.NatRepr n -> DBS.BV n -> App f (BVType n)
  LitString :: T.Text -> App f StringType
  LitFloat :: FloatPrecRepr p -> T.Text -> Double -> App f (FloatType p)
  LitBool :: Bool -> App f BoolType

  And :: f BoolType -> f BoolType -> App f BoolType
  Or :: f BoolType -> f BoolType -> App f BoolType
  Xor :: f BoolType -> f BoolType -> App f BoolType
  Not :: f BoolType -> App f BoolType

  Eq :: f tp -> f tp -> App f BoolType
  Ne :: f tp -> f tp -> App f BoolType

  BVNeg :: PN.NatRepr n -> f (BVType n) -> App f (BVType n)
  BVAdd :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVSub :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVMul :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVUDiv :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVUMod :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVSDiv :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVSMod :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVAnd :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVOr :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVXor :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVShl :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVAshr :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVLshr :: PN.NatRepr n -> f (BVType n) -> f (BVType n) -> App f (BVType n)
  BVSlt :: f (BVType n) -> f (BVType n) -> App f BoolType
  BVSle :: f (BVType n) -> f (BVType n) -> App f BoolType
  BVUlt :: f (BVType n) -> f (BVType n) -> App f BoolType
  BVUle :: f (BVType n) -> f (BVType n) -> App f BoolType
  BVToFloat :: PN.NatRepr n -> f (BVType n) -> FloatPrecRepr p -> App f (FloatType p)

  FNeg :: FloatPrecRepr p -> f (FloatType p) -> App f (FloatType p)
  FAdd :: FloatPrecRepr p -> f (FloatType p) -> f (FloatType p) -> App f (FloatType p)
  FSub :: FloatPrecRepr p -> f (FloatType p) -> f (FloatType p) -> App f (FloatType p)
  FMul :: FloatPrecRepr p -> f (FloatType p) -> f (FloatType p) -> App f (FloatType p)
  FDiv :: FloatPrecRepr p -> f (FloatType p) -> f (FloatType p) -> App f (FloatType p)
  FRem :: FloatPrecRepr p -> f (FloatType p) -> f (FloatType p) -> App f (FloatType p)
  FLt :: f (FloatType p) -> f (FloatType p) -> App f BoolType
  FLe :: f (FloatType p) -> f (FloatType p) -> App f BoolType
  FGt :: f (FloatType p) -> f (FloatType p) -> App f BoolType
  FGe :: f (FloatType p) -> f (FloatType p) -> App f BoolType
  FloatToBV :: FloatPrecRepr p -> f (FloatType p) -> PN.NatRepr n -> App f (BVType n)

  Call :: Repr tp -> T.Text -> Ctx.Assignment (App f) tps -> App f tp

data Stmt globals locals where
  SetReg :: Ctx.Index locals tp -> Expr globals locals tp -> Stmt globals locals
  WriteGlobal :: Ctx.Index globals tp -> Reg globals locals tp -> Stmt globals locals

data GlobalVariable tp where
  GlobalVariable :: Repr tp -> T.Text -> GlobalVariable tp

deriving instance Show (GlobalVariable tp)

data LocalVariable tp where
  LocalVariable :: Repr tp -> T.Text -> LocalVariable tp
  TemporaryVariable :: Repr tp -> Natural -> LocalVariable tp

globalVarRepr :: GlobalVariable tp -> Repr tp
globalVarRepr (GlobalVariable r _) = r

globalVarName :: GlobalVariable tp -> T.Text
globalVarName (GlobalVariable _ t) = t

localVarRepr :: LocalVariable tp -> Repr tp
localVarRepr v =
  case v of
    LocalVariable r _ -> r
    TemporaryVariable r _ -> r

data Probe globals where
  Probe :: DLN.NonEmpty LDP.ProbeDescription
        -> Expr globals locals BoolType
        -> Ctx.Assignment LocalVariable locals
        -> [Stmt globals locals]
        -> Probe globals

data Probes where
  Probes :: Ctx.Assignment GlobalVariable globals
         -> Map.Map T.Text (Some (Ctx.Index globals))
         -> [Probe globals]
         -> Probes
