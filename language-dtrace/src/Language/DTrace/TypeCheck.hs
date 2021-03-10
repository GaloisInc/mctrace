{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Language.DTrace.TypeCheck (
    typeCheck
  , TypeErrorMessage(..)
  , TypeError(..)
  ) where

import           Control.Applicative ( (<|>) )
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Unsafe.Coerce ( unsafeCoerce )

import qualified Language.DTrace.LexerWrapper as LDL
import qualified Language.DTrace.Syntax.Typed as ST
import qualified Language.DTrace.Syntax.Untyped as SU


data TypeErrorMessage where
  IndeterminateInitializerType :: SU.Expr -> TypeErrorMessage
  TypeErrorMessage :: TypeErrorMessage

data TypeError where
  TypeError :: LDL.SourceRange -> TypeErrorMessage -> TypeError

typeError :: (LDL.HasRange a) => a -> TypeErrorMessage -> TypeError
typeError loc = TypeError (LDL.range loc)

data PState globals locals =
  PState { globalVars :: !(Ctx.Assignment ST.Variable globals)
         -- ^ Definitions of all of the global variables accessed in the script
         , globalMap :: !(Map.Map T.Text (Some (Ctx.Index globals)))
         -- ^ A map to look up variables in 'globalVars' by name
         , translatedProbes :: Seq.Seq (ST.Probe globals)
         -- ^ All of the probes that have been translated so far
         , localVars :: !(Ctx.Assignment ST.Variable locals)
         -- ^ The locals for the *current* probe being translated
         , localMap :: !(Map.Map T.Text (Some (Ctx.Index locals)))
         -- ^ A map to look up variables in 'localVars' by name
         , probeStmts :: !(Seq.Seq (ST.Stmt globals locals))
         -- ^ The statements translated so far for the current probe
         --
         -- Note that these are in a-normal form (i.e., simple three address
         -- code without nested expressions)
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
    SU.LongLongTy -> Some (ST.BVRepr (PN.knownNat @64))
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
    SU.Cast (LDL.Located _loc t) _ -> Just (typeRepr t)
    SU.Ternary _ e1 e2 -> typeOfExpr (LDL.value e1) <|> typeOfExpr (LDL.value e2)


-- | Return true if the given variable name is defined in the global state
--
-- This is pure to support use in pattern guards
undefinedVar :: PState globals locals -> T.Text -> Bool
undefinedVar s name = Map.member name (globalMap s)

resetLocals :: State -> State
resetLocals (State pstate) =
  State PState { globalVars = globalVars pstate
               , globalMap = globalMap pstate
               , translatedProbes = translatedProbes pstate
               , localVars = Ctx.empty
               , localMap = Map.empty
               , probeStmts = mempty
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
         }

addGlobal :: T.Text -> ST.Repr tp -> State -> State
addGlobal varName varRepr (State pstate) =
  State PState { globalVars = xvars
               , globalMap = xmap
               , translatedProbes = unsafeCoerce (translatedProbes pstate)
               , localVars = localVars pstate
               , localMap = localMap pstate
               , probeStmts = unsafeCoerce (probeStmts pstate)
               }
  where
    var = ST.Variable varRepr varName
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
              Nothing -> RWS.tell (Seq.singleton (typeError rhs (IndeterminateInitializerType (LDL.value rhs))))
        _ -> return ()
      app' <- SU.traverseExpr allocateGlobalLValues app
      return (LDL.Located range (SU.Expr app'))

translateStatement :: LDL.Located SU.Stmt -> TC ()
translateStatement stmt = return ()

checkProbe :: SU.TopLevel -> TC ()
checkProbe tu =
  case tu of
    SU.TopDecl {} -> return ()
    SU.TopProbe p -> do
      RWS.modify' resetLocals

      mapM_ translateStatement (SU.probeBody (LDL.value p))

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
typeCheck :: [SU.TopLevel] -> Either [TypeError] ST.Probes
typeCheck topLevels =
  case RWS.execRWS (unTC (doTypeCheck topLevels)) () emptyState of
    (State finalPState, errs)
      | Seq.null errs -> Right (ST.Probes (globalVars finalPState) (F.toList (translatedProbes finalPState)))
      | otherwise -> Left (F.toList errs)
