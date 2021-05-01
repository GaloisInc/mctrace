{-# LANGUAGE RankNTypes #-}
module MCTrace.ProbeProvider (
    ProbeProvider(..)
  , ProbeInserter(..)
  ) where

import qualified Data.List.NonEmpty as DLN
import qualified Prettyprinter as PP
import qualified Renovate as R

import qualified Language.DTrace as LD
import qualified MCTrace.Analysis as MA

-- | A function to insert a probe into a basic block (represented as a stream of instructions)
newtype ProbeInserter arch =
  ProbeInserter { insertProbe :: forall tp
                               . R.InstructionArchRepr arch tp
                              -> DLN.NonEmpty (R.Instruction arch tp (R.Relocation arch))
                              -> DLN.NonEmpty (R.Instruction arch tp (R.Relocation arch))
                }

instance Semigroup (ProbeInserter arch) where
  ProbeInserter i1 <> ProbeInserter i2 =
    ProbeInserter $ \irep insns -> i2 irep (i1 irep insns)

instance Monoid (ProbeInserter arch) where
  mempty = ProbeInserter $ \_irep insns -> insns

-- | Each probe provider identifies locations in blocks that it can be applied;
-- if there is a user-provided probe definition (held in the
-- 'MA.ProbeLocationAnalysisResult'), and the provider applies, it will produce
-- a 'ProbeInserter'
data ProbeProvider globals arch =
  ProbeProvider { providerName :: LD.ProbeDescription
                , providerDescription :: PP.Doc ()
                , providerMatcher :: MA.ProbeLocationAnalysisResult globals arch
                                  -> R.SymbolicBlock arch
                                  -> Maybe (ProbeInserter arch)
                -- ^ A function that returns a 'ProbeInserter' if this provider
                -- can be applied to the given 'R.SymbolicBlock'
                --
                -- The matcher gets the analysis results that indexed various
                -- things necessary to identify probe insertion points
                }
