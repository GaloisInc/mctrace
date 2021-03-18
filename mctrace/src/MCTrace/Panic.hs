{-# LANGUAGE TemplateHaskell #-}
module MCTrace.Panic (
    MCTraceComponent(..)
  , panic
  ) where

import qualified Panic as P

data MCTraceComponent = LLVMCodegen
  deriving (Show)

instance P.PanicComponent MCTraceComponent where
  panicComponentName = show
  panicComponentIssues _ = "https://gitlab-ext.galois.com/tristan/mctrace/-/issues"
  panicComponentRevision = $(P.useGitRevision)

panic :: MCTraceComponent -> String -> [String] -> a
panic = P.panic
