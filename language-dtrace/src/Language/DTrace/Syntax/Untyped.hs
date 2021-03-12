{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.DTrace.Syntax.Untyped (
    Type(..)
  , App(..)
  , Expr(..)
  , Stmt(..)
  , Probe(..)
  , TopLevel(..)
  , traverseExpr
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Text as T
import qualified Data.Traversable as DT
import           Numeric.Natural ( Natural )
import qualified Prettyprinter as PP

import           Language.DTrace.LexerWrapper ( Located(..) )
import qualified Language.DTrace.ProbeDescription as LDP
import qualified Language.DTrace.Token as DT

data Type where
  CharTy :: Type
  ShortTy :: Type
  IntTy :: Type
  SignedTy :: Type
  UnsignedTy :: Type
  LongTy :: Type
  LongLongTy :: Type
  ULongTy :: Type
  ULongLongTy :: Type
  StringTy :: Type
  FloatTy :: Type
  DoubleTy :: Type

deriving instance Show Type
deriving instance Eq Type
deriving instance Ord Type

data App f where
  This :: App f
  Self :: App f
  LitInt :: DT.NumericLiteralFormat -> Natural -> App f
  LitLong :: Natural -> App f
  LitULong :: Natural -> App f
  LitLongLong :: Natural -> App f
  LitULongLong :: Natural -> App f
  LitString :: T.Text -> App f
  LitDouble :: T.Text -> Double -> App f
  LitFloat :: T.Text -> Float -> App f
  VarRef :: T.Text -> App f
  FieldRef :: f -> T.Text -> App f

  Cast :: Located Type -> f -> App f

  Assign :: f -> f -> App f

  And :: f -> f -> App f
  Or :: f -> f -> App f
  Xor :: f -> f -> App f
  Not :: f -> App f

  Eq :: f -> f -> App f
  Ne :: f -> f -> App f
  Lt :: f -> f -> App f
  Le :: f -> f -> App f
  Gt :: f -> f -> App f
  Ge :: f -> f -> App f

  Neg :: f -> App f
  Add :: f -> f -> App f
  Sub :: f -> f -> App f
  Mul :: f -> f -> App f
  Div :: f -> f -> App f
  Mod :: f -> f -> App f

  BVNeg :: f -> App f
  BVAnd :: f -> f -> App f
  BVOr :: f -> f -> App f
  BVXor :: f -> f -> App f
  BVShl :: f -> f -> App f
  BVLshr :: f -> f -> App f
  BVAshr :: f -> f -> App f

  Ternary :: f -> f -> f -> App f
  Call :: T.Text -> [f] -> App f

deriving instance (Show f) => Show (App f)
deriving instance F.Foldable App
deriving instance Functor App

newtype Expr = Expr { exprApp :: App (Located Expr) }
  deriving (Show)

data Stmt where
  DeclStmt :: Located Type -> T.Text -> Stmt
  ExprStmt :: Located Expr -> Stmt

deriving instance Show Stmt

data Probe =
  Probe { probePatterns :: DLN.NonEmpty (Located LDP.ProbeDescription)
        , probeBody :: [Located Stmt]
        }
  deriving (Show)

data TopLevel =
  TopDecl (Located Type) T.Text
  | TopProbe (Located Probe)
  deriving (Show)


traverseExpr :: (Monad m) => (f -> m f') -> App f -> m (App f')
traverseExpr f app =
  case app of
    This -> pure This
    Self -> pure Self
    LitInt nlf n -> pure (LitInt nlf n)
    LitLong n -> pure (LitLong n)
    LitULong n -> pure (LitULong n)
    LitLongLong n -> pure (LitLongLong n)
    LitULongLong n -> pure (LitULongLong n)
    LitString t -> pure (LitString t)
    LitDouble t d -> pure (LitDouble t d)
    LitFloat t fl -> pure (LitFloat t fl)
    VarRef t -> pure (VarRef t)
    FieldRef e t -> FieldRef <$> f e <*> pure t

    Cast t e -> Cast t <$> f e

    Assign lhs rhs -> Assign <$> f lhs <*> f rhs

    And lhs rhs -> And <$> f lhs <*> f rhs
    Or lhs rhs -> And <$> f lhs <*> f rhs
    Xor lhs rhs -> And <$> f lhs <*> f rhs
    Not e -> Not <$> f e

    Eq lhs rhs -> Eq <$> f lhs <*> f rhs
    Ne lhs rhs -> Ne <$> f lhs <*> f rhs
    Lt lhs rhs -> Lt <$> f lhs <*> f rhs
    Le lhs rhs -> Le <$> f lhs <*> f rhs
    Gt lhs rhs -> Gt <$> f lhs <*> f rhs
    Ge lhs rhs -> Ge <$> f lhs <*> f rhs

    Neg e -> Neg <$> f e
    Add lhs rhs -> Add <$> f lhs <*> f rhs
    Sub lhs rhs -> Sub <$> f lhs <*> f rhs
    Mul lhs rhs -> Mul <$> f lhs <*> f rhs
    Div lhs rhs -> Div <$> f lhs <*> f rhs
    Mod lhs rhs -> Mod <$> f lhs <*> f rhs

    BVNeg e -> BVNeg <$> f e
    BVAnd lhs rhs -> BVAnd <$> f lhs <*> f rhs
    BVOr lhs rhs -> BVOr <$> f lhs <*> f rhs
    BVXor lhs rhs -> BVXor <$> f lhs <*> f rhs
    BVShl lhs rhs -> BVShl <$> f lhs <*> f rhs
    BVLshr lhs rhs -> BVLshr <$> f lhs <*> f rhs
    BVAshr lhs rhs -> BVAshr <$> f lhs <*> f rhs

    Ternary e1 e2 e3 -> Ternary <$> f e1 <*> f e2 <*> f e3
    Call t args -> Call t <$> DT.traverse f args
