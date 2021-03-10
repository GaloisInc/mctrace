{-# LANGUAGE GADTs #-}
module Main ( main ) where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Numeric.Natural ( Natural )
import qualified System.FilePath as SF
import qualified System.FilePath.Glob as SFG
import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as TTH
import           Text.Read ( readMaybe )

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT

data DType = BoolType
           | BVType Natural
           | StringType
           deriving (Read, Show)

data Expected =
  Expected { globals :: [(String, DType)]
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
    _ -> False

mkTypecheckTest :: FilePath -> TT.TestTree
mkTypecheckTest expectedFile = TTH.testCase dfile $ do
  expectedString <- readFile expectedFile
  case readMaybe expectedString of
    Nothing -> TTH.assertFailure ("Error parsing expected file: " ++ expectedFile)
    Just ex -> do
      dcontent <- TIO.readFile dfile
      case LD.parseDTrace dfile dcontent of
        Left err -> TTH.assertFailure ("Error parsing DTrace file: " ++ show err)
        Right decls ->
          case LD.typeCheck decls of
            Left errs -> TTH.assertFailure ("Type errors: " ++ show errs)
            Right (LDT.Probes globalVars globalMap _probes) -> do
              F.forM_ (globals ex) $ \(name, exTy) -> do
                case Map.lookup (T.pack name) globalMap of
                  Nothing -> TTH.assertFailure ("No global entry for expected global: " ++ name)
                  Just (Some idx) -> do
                    let LDT.Variable rep _name = globalVars Ctx.! idx
                    TTH.assertBool "Expected Type" (expectedType exTy rep)
              F.forM_ (Map.keys globalMap) $ \actualGlobalName -> do
                case lookup (T.unpack actualGlobalName) (globals ex) of
                  Nothing -> TTH.assertFailure ("Discovered unexpected global: " ++ show actualGlobalName)
                  Just _ -> return ()
  where
    dfile = SF.dropExtension expectedFile


typecheckTests :: [FilePath] -> TT.TestTree
typecheckTests = TT.testGroup "TypeCheck" . map mkTypecheckTest

main :: IO ()
main = do
  tcTestExpected <- SFG.namesMatching "tests/tc/*.d.expected"
  let tests = TT.testGroup "DTrace" [ typecheckTests tcTestExpected
                                    ]
  TT.defaultMain tests
