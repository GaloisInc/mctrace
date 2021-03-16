{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import qualified System.FilePath.Glob as SFG
import qualified Test.Tasty as TT

import           Eval ( mkEvalTest )
import           TypeCheck ( mkTypecheckTest )


typecheckTests :: [FilePath] -> TT.TestTree
typecheckTests = TT.testGroup "TypeCheck" . map mkTypecheckTest

evalTests :: [FilePath] -> TT.TestTree
evalTests = TT.testGroup "Eval" . map mkEvalTest

main :: IO ()
main = do
  tcTestExpected <- SFG.namesMatching "tests/tc/*.d.expected"
  evalTestExpected <- SFG.namesMatching "tests/eval/*.d.expected"
  let tests = TT.testGroup "DTrace" [ typecheckTests tcTestExpected
                                    , evalTests evalTestExpected
                                    ]
  TT.defaultMain tests
