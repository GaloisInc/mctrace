{-# LANGUAGE GADTs #-}
module TypeCheck ( mkTypecheckTest ) where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Numeric.Natural ( Natural )
import qualified System.FilePath as SF
import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as TTH
import           Text.Read ( readMaybe )

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT

data DType = BoolType
           | BVType Natural
           | FloatType
           | DoubleType
           | StringType
           deriving (Read, Show)

data TCExpected =
  TCExpected { globals :: [(String, DType)]
             }
  deriving (Read, Show)

-- | Return True if the given expected type (from the test description) matches
-- the parsed out type
--
-- These are separate types because we can't get a Read instance easily from the
-- type repr.
expectedType :: DType -> LDT.Repr tp -> Bool
expectedType dt rep =
  case (dt, rep) of
    (BoolType, LDT.BoolRepr) -> True
    (StringType, LDT.StringRepr) -> True
    (BVType n, LDT.BVRepr nr) -> n == PN.natValue nr
    (FloatType, LDT.FloatRepr LDT.SinglePrecRepr) -> True
    (DoubleType, LDT.FloatRepr LDT.DoublePrecRepr) -> True
    _ -> False

-- | The expected output of these tests records the global variables (and their
-- types) that the type checker is expected to have inferred.
--
-- This test ensures that the expected variables are present and that no
-- unexpected variables are present
mkTypecheckTest :: FilePath -> TT.TestTree
mkTypecheckTest expectedFile = TTH.testCase dfile $ do
  dcontent <- TIO.readFile dfile
  case LD.parseDTrace dfile dcontent of
    Left err -> TTH.assertFailure ("Error parsing DTrace file: " ++ show err)
    Right decls ->
      case LD.typeCheck decls of
        Left errs -> TTH.assertFailure ("Type errors: " ++ show errs)
        Right (LDT.Probes globalVars globalMap _probes) -> do
          expectedString <- readFile expectedFile
          case readMaybe expectedString of
            Nothing -> TTH.assertFailure ("Error parsing expected file: " ++ expectedFile)
            Just ex -> do
              F.forM_ (globals ex) $ \(name, exTy) -> do
                case Map.lookup (T.pack name) globalMap of
                  Nothing -> TTH.assertFailure ("No global entry for expected global: " ++ name)
                  Just (Some idx) -> do
                    let LDT.GlobalVariable rep _name = globalVars Ctx.! idx
                    TTH.assertBool ("Expected type " ++ show exTy ++ " but got " ++ show rep) (expectedType exTy rep)
              F.forM_ (Map.keys globalMap) $ \actualGlobalName -> do
                case lookup (T.unpack actualGlobalName) (globals ex) of
                  Nothing -> TTH.assertFailure ("Discovered unexpected global: " ++ show actualGlobalName)
                  Just _ -> return ()
  where
    dfile = SF.dropExtension expectedFile
