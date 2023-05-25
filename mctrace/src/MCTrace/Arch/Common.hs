module MCTrace.Arch.Common
  ( symAddressForSymbolPattern
  )
where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Renovate as R

import qualified Language.DTrace.ProbeDescription as LDP
import qualified MCTrace.Analysis as MA

symAddressForSymbolPattern
  :: MA.ProbeLocationAnalysisResult globals arch
  -> LDP.ProbeComponent
  -> [R.SymbolicAddress arch]
symAddressForSymbolPattern locationAnalysis symPattern = do
    -- Find all (symbolic addresses of) all functions in the binary that
    -- match the given pattern
    let lookupSymbolicAddr entryAddr = Map.findWithDefault (R.stableAddress entryAddr) entryAddr (MA.symbolicAddresses locationAnalysis)
    map lookupSymbolicAddr matches
    where
      checkForMatch pattern fnName _ = LDP.matchWithPattern pattern (T.pack $ BSC.unpack fnName)
      entryPoints = MA.functionEntryPoints locationAnalysis
      matches = case symPattern of
          LDP.FreePattern -> Map.elems entryPoints
          LDP.SymbolPattern {} -> Map.elems $ Map.filterWithKey (checkForMatch symPattern) entryPoints
          LDP.Identifier ident -> maybe [] (:[]) $ Map.lookup (BSC.pack $ T.unpack ident) entryPoints
