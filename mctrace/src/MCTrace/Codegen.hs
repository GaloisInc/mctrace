{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- | Code for managing probes across any compilation backend
module MCTrace.Codegen (
    BinaryWord
  , ProbeIndex(..)
  , indexELFProbes
  ) where

import qualified Control.Lens as L
import           Control.Monad ( unless )
import qualified Control.Monad.Except as CME
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ElfEdit as DE
import qualified Data.Foldable as F
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Parameterized.NatRepr as PN
import           Data.Word ( Word32, Word64 )

import qualified Language.DTrace.Syntax.Typed as LDT
import qualified MCTrace.Exceptions as ME

type family BinaryWord w where
  BinaryWord 32 = Word32
  BinaryWord 64 = Word64

binaryWordRepr :: DE.ElfClass w -> PN.NatRepr w
binaryWordRepr k =
  case k of
    DE.ELFCLASS32 -> PN.knownNat @32
    DE.ELFCLASS64 -> PN.knownNat @64

-- | An index of probes to the code implementing them
--
-- This attempts to abstract away any ELF-specific details in case we support
-- other container formats
data ProbeIndex globals w =
  ProbeIndex { pointerWidth :: PN.NatRepr w
             -- ^ The width of pointers on this architecture
             , probeOffsets :: [(LDT.Probe globals, String, BS.ByteString)]
             -- ^ Offsets of each user-defined probe from the beginning of the
             -- text section allocated to the probe machine code
             , probeStorageFile :: FilePath
             -- ^ The file that mctrace global storage should be backed by at run time
             , probeStorageBytes :: Word32
             -- ^ The number of bytes required for global storage
             }

-- | Split the bytestring such that the bytes for each probe definition are
-- paired with it
--
-- This starts by sorting the probes by their offset so that we can recursively
-- consume the list (and also peek at the next probe to know where to stop
-- taking bytes)
splitProbeBytes
  :: (Integral w, Ord w)
  => BS.ByteString
  -- ^ The bytes of the text section containing probe code
  -> [(LDT.Probe globals, String, w)]
  -- ^ The list of probes along with their offsets
  -> [(LDT.Probe globals, String, BS.ByteString)]
splitProbeBytes probeBytes = go [] . DL.sortOn (L.^. L._3)
  where
    go acc [] = reverse acc
    go acc [(p, probeSymbol, off)] = reverse ((p, probeSymbol, BS.drop (fromIntegral off) probeBytes) : acc)
    go acc ((p, probeSymbol, off) : next@(_, _, nextOff) : rest) =
      let withoutPrefix = BS.drop (fromIntegral off) probeBytes
          justFunc = BS.take (fromIntegral nextOff - fromIntegral off) withoutPrefix
      in go ((p, probeSymbol, justFunc) : acc) (next : rest)


-- | Compute the offset of each probe from the base of the section containing
-- the probe code.  The offsets will be used to compute the extents of each
-- probe function to break it into individual bytestrings.
translateProbeAddress
  :: (Num w)
  => w
  -- ^ The base address of the section containing the probe code
  -> Map.Map BS8.ByteString (DE.SymtabEntry BS8.ByteString w)
  -- ^ The symbol table (indexed by name)
  -> (LDT.Probe globals, String)
  -- ^ The probe (and name of the probe) we want to add to the map
  -> CME.Except ME.TraceException (LDT.Probe globals, String, w)
translateProbeAddress baseAddr symtab (probe, probeName) =
  case Map.lookup (BS8.pack probeName) symtab of
    Nothing -> CME.throwError (ME.MissingProbeSymbol probeName)
    Just entry -> return (probe, probeName, DE.steValue entry - baseAddr)

withElfClassConstraints
  :: DE.ElfHeaderInfo w
  -> ((Integral (BinaryWord w), Ord (BinaryWord w), Num (DE.ElfWordType w), DE.ElfWordType w ~ BinaryWord w) => a)
  -> a
withElfClassConstraints ehi k =
  case DE.headerClass (DE.header ehi) of
    DE.ELFCLASS32 -> k
    DE.ELFCLASS64 -> k

-- | Compute the mapping of probes to their offsets in the text section of the
-- provided 'DE.ElfHeaderInfo' file.  The mapping is performed based on the names of
-- the probes.
--
-- This function will fail if any probes cannot be found in the ELF file
indexELFProbes
  :: [(LDT.Probe globals, String)]
  -- ^ The user-defined probes
  -> DE.ElfHeaderInfo w
  -- ^ The ELF file to index
  -> FilePath
  -- ^ The file to store probes to at run time
  -> Word32
  -- ^ The number of bytes of probe storage required
  -> Either ME.TraceException (ProbeIndex globals w)
indexELFProbes probes ehi storageFile storageBytes = CME.runExcept $ withElfClassConstraints ehi $ do
  let (errs, elf) = DE.getElf ehi
  unless (null errs) $ do
    CME.throwError (ME.ELFParseError errs)
  symbolTable <- case DE.elfSymtab elf of
    [] -> CME.throwError (ME.MissingGeneratedProbeSection ".symtab")
    [symtab] -> return symtab
    _ -> CME.throwError (ME.MultipleGeneratedProbeSections ".symtab")

  textSec <- case DE.findSectionByName (BS8.pack ".text") elf of
    [] -> CME.throwError (ME.MissingGeneratedProbeSection ".text")
    [textSection] -> return textSection
    _ -> CME.throwError (ME.MultipleGeneratedProbeSections ".text")

  -- We want to compute the offset into the text section of each symbol we care
  -- about.  To do that, we need the virtual address of the .text section and
  -- all of the entries from the symbol table that are relevant
  --
  -- FIXME: We need to be a bit tricky here for PowerPC64 (if we want to support
  -- it) where symbol table entries are actually the address of TOC entries (and
  -- the TOC entries contain the actual function addresses)
  let baseAddr = DE.elfSectionAddr textSec

  let symbolIndex = Map.fromList [ (DE.steName entry, entry)
                                 | entry <- F.toList (DE.symtabEntries symbolTable)
                                 ]
  let probeBytes = DE.elfSectionData textSec
  offsets <- mapM (translateProbeAddress baseAddr symbolIndex) probes
  let ptrRepr = binaryWordRepr (DE.headerClass (DE.header ehi))
  return $ ProbeIndex { pointerWidth = ptrRepr
                      , probeOffsets = splitProbeBytes probeBytes offsets
                      , probeStorageFile = storageFile
                      , probeStorageBytes = storageBytes
                      }
