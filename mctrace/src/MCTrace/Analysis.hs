module MCTrace.Analysis (
    LogEvent(..)
  , InjectedAssets(..)
  , ProbeLocationAnalysisResult(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Language.DTrace.Syntax.Typed as LDT
import qualified Renovate as R

-- | The type of events that can be generated by the binary rewriter that are application-specific
data LogEvent = LogEvent
  deriving (Show)

-- | Information collected in the pre-analysis phase
data InjectedAssets globals arch =
  InjectedAssets { injectedStorePointer :: R.ConcreteAddress arch
                 -- ^ The (symbolic) address of storage for the global pointer
                 -- that points to the global memory for the probes
                 --
                 -- The actual memory will be allocated at run-time (via mmap),
                 -- but this pointer gives us a location to store the actual
                 -- address and to access it from at run-time
                 --
                 -- FIXME: Does this need to be a symbolic address?
                 , injectedProbeAddrs :: [(LDT.Probe globals, R.SymbolicAddress arch)]
                 -- ^ The symbolic address assigned to each probe
                 }

-- | This is the result produced from the (architecture-specific) analysis of
-- the binary before the rewriting phase.
--
-- This will ultimately contain the list of blocks (and offsets into blocks)
-- that need to be instrumented with a probe.
data ProbeLocationAnalysisResult globals arch =
  ProbeLocationAnalysisResult { injectedAssets :: InjectedAssets globals arch
                              , functionEntryPoints :: Map.Map BS.ByteString (R.ConcreteAddress arch)
                              -- ^ An index of function names to their entry points
                              }