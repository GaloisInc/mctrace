module Parse
  ( mkParseTest
  )
where

import qualified Data.Text.IO as TIO
import qualified System.FilePath as SF
import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as TTH

import qualified Language.DTrace as LD

mkParseTest :: FilePath -> TT.TestTree
mkParseTest expectedFile =
    let inputFile = SF.dropExtension expectedFile
    in TTH.testCase inputFile $ do
        expected <- readFile expectedFile
        src <- TIO.readFile inputFile
        case LD.parseDTrace inputFile src of
            Left err -> TTH.assertFailure ("Error parsing DTrace file: " ++ show err)
            Right actual -> do
                TTH.assertEqual "" expected (show actual <> "\n")
