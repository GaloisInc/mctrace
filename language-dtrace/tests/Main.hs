{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import qualified System.FilePath.Glob as SFG
import qualified Test.Tasty as TT

import           Eval ( mkEvalTest )
import           TypeCheck ( mkTypecheckTest )
import           Parse ( mkParseTest )


typecheckTests :: [FilePath] -> TT.TestTree
typecheckTests = TT.testGroup "TypeCheck" . map mkTypecheckTest

evalTests :: [FilePath] -> TT.TestTree
evalTests = TT.testGroup "Eval" . map mkEvalTest

parseTests :: [FilePath] -> TT.TestTree
parseTests = TT.testGroup "Parse" . map mkParseTest

main :: IO ()
main = do
  parseTestExpected <- SFG.namesMatching "tests/parse/*.d.expected"
  tcTestExpected <- SFG.namesMatching "tests/tc/*.d.expected"
  evalTestExpected <- SFG.namesMatching "tests/eval/*.d.expected"
  let tests = TT.testGroup "DTrace" [ parseTests parseTestExpected
                                    , typecheckTests tcTestExpected
                                    , evalTests evalTestExpected
                                    ]
  TT.defaultMain tests
