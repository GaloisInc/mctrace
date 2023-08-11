{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module MCTrace.Arch.Common
  ( injectPlatformApiImpl
  , symAddressForSymbolPattern
  , SymbolOffset
  , SymbolLocation(..)
  )
where

import           Control.Monad ( unless )
import qualified Control.Monad.Catch as X
import qualified Control.Monad.Except as CME
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as EE
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Data.Tuple (swap)
import qualified Renovate as R

import qualified Language.DTrace.ProbeDescription as LDP
import qualified MCTrace.Analysis as MA
import qualified MCTrace.Codegen as MC
import qualified MCTrace.Exceptions as ME
import qualified MCTrace.RuntimeAPI as RT

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


newtype SymbolOffset = SymbolOffset Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data SymbolLocation arch = SymbolLocation { slSymbolBaseAddress :: R.SymbolicAddress arch 
                                          , slSymbolOffset :: SymbolOffset
                                          }

injectPlatformApiImpl
  :: EE.ElfHeaderInfo n
  -> Map.Map RT.SupportFunction String
  -> R.RewriteM MA.LogEvent arch (Map.Map RT.SupportFunction (SymbolLocation arch))
injectPlatformApiImpl library supportFunNames = do
  -- Inject the platform API implementation and fetch the locations of
  -- the required support functions

  -- Extract the text section and the symbol table
  (textSec, symbolTable) <- case getTextSectionAndSymbolTable library of
    Left err -> X.throwM err
    Right v -> return v
  
  -- Use that to figure out where the required support functions are
  -- with respect to the start of the text section
  let supportFunctionOffsetMap = indexFunctions library textSec symbolTable
  
  -- Check if any support functions are missing
  let missingFns = Map.difference supportFunNames supportFunctionOffsetMap
  unless (Map.null missingFns) $
    X.throwM (ME.MissingSupportFunction (show <$> Map.keys missingFns))
  
  -- Inject the whole text section in to the binary
  let bytes = EE.elfSectionData textSec
  symAddress <- R.injectFunction "__mctrace_platform_api_impl" bytes
  
  -- Return a mapping of support functions
  return $ Map.map (mkLocation symAddress) supportFunctionOffsetMap
  where
    getTextSectionAndSymbolTable :: EE.ElfHeaderInfo n -> Either ME.TraceException (EE.ElfSection (EE.ElfWordType n), EE.Symtab n)
    getTextSectionAndSymbolTable lib = CME.runExcept $ {- MC.withElfClassConstraints lib $ -} do
      -- Fetch the symbol table and text section from the given binary and
      -- throw errors if we do not find what we are looking for
      let (errs, elf) = EE.getElf lib
      unless (null errs) $ do
        CME.throwError (ME.ELFParseError errs)    
      symbolTable <- case EE.elfSymtab elf of
        [] -> CME.throwError (ME.MissingGeneratedProbeSection ".symtab")
        [symtab] -> return symtab
        _ -> CME.throwError (ME.MultipleGeneratedProbeSections ".symtab")
      textSec <- case EE.findSectionByName (BSC.pack ".text") elf of
        [] -> CME.throwError (ME.MissingGeneratedProbeSection ".text")
        [textSection] -> return textSection
        _ -> CME.throwError (ME.MultipleGeneratedProbeSections ".text")
      return (textSec, symbolTable)        
    indexFunctions :: EE.ElfHeaderInfo n -> EE.ElfSection (EE.ElfWordType n) -> EE.Symtab n -> Map.Map RT.SupportFunction Int
    indexFunctions lib textSec symbolTable = MC.withElfClassConstraints lib $ do
      -- Compute the relative location (with respect to the start of the text section)
      -- for each support function
      let baseAddr = EE.elfSectionAddr textSec
          nameMap = Map.fromList $ swap <$> Map.toList supportFunNames
          symbolOffsetPairs = [ (nameMap Map.! entryName, fromIntegral (EE.steValue entry - baseAddr))
                              | entry <- F.toList (EE.symtabEntries symbolTable)
                              , let entryName = BSC.unpack (EE.steName entry)
                              , Map.member entryName nameMap
                              ]
      Map.fromList symbolOffsetPairs
    mkLocation symAddress offset = SymbolLocation { slSymbolBaseAddress = symAddress
                                                  , slSymbolOffset = SymbolOffset offset 
                                                  }