{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module MCTrace.PLT (
    ArchRelocationType
  , pltStubSymbols
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as EE
import qualified Data.ElfEdit.Prim as EEP
import qualified Data.Foldable as F
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Macaw.CFG as DMC
import qualified Data.Macaw.Memory as DMM
import qualified Data.Macaw.Memory.LoadCommon as DMML
import           Data.Macaw.X86.Symbolic ()
import qualified Data.Map.Strict as Map
import           Data.Maybe ( fromMaybe, listToMaybe )
import           Data.Proxy ( Proxy(..) )
import           Data.Word ( Word16, Word32 )
import qualified Renovate as R

pltStubAddresses
  :: EEP.DynamicSection w
  -> EEP.VirtAddrMap w
  -> EEP.VersionDefMap
  -> EEP.VersionReqMap
  -> (t -> Word32)
  -> [EEP.SymtabEntry BSC.ByteString (EEP.ElfWordType w)]
  -> t
  -> [EEP.SymtabEntry BSC.ByteString (EEP.ElfWordType w)]
pltStubAddresses dynSec vam vdefm vreqm getRelSymIdx accum rel
  | Right (symtabEntry, _versionedVal) <- EEP.dynSymEntry dynSec vam vdefm vreqm (getRelSymIdx rel) =
      symtabEntry : accum
  | otherwise = accum

buildAssocList
  :: (DMM.MemWidth w)
  => [(Integer, b)]
  -> Integer
  -> Integer
  -> DMML.LoadOptions
  -> [(DMM.MemWord w, b)]
buildAssocList nameRelaMap baseAddr stubSize loadOptions =
  [ (DMM.memWord (fromIntegral addr), sym)
  | (idx, sym) <- nameRelaMap
  , let addr = loadOffset + baseAddr + idx * stubSize
  ]
  where
    loadOffset = toInteger (fromMaybe 0 (DMML.loadOffset loadOptions))

data SomeRel tp where
  SomeRel :: [r tp] -> (r tp -> Word32) -> SomeRel tp

extractPltAddrs
  :: forall proxy tp w
   . ( EEP.IsRelocationType tp
     , DMM.MemWidth w
     , w ~ EEP.RelocationWidth tp
     , Integral (EEP.ElfWordType w)
     )
  => proxy tp
  -> EEP.DynamicSection (EEP.RelocationWidth tp)
  -> EEP.VirtAddrMap (EEP.RelocationWidth tp)
  -> Map.Map Word16 EEP.VersionId
  -> Map.Map Word16 EEP.VersionId
  -> DMML.LoadOptions
  -> EE.Elf w
  -> Maybe [( DMM.MemWord w
            , EEP.SymtabEntry BSC.ByteString (EEP.ElfWordType (EEP.RelocationWidth tp)))
           ]
extractPltAddrs _ dynSec vam vdefm vreqm loadOptions elf = do
  SomeRel rels getRelSymIdx <- case EEP.dynPLTRel @tp dynSec vam of
    Right (EEP.PLTRela relas) -> return (SomeRel relas EEP.relaSym)
    Right (EEP.PLTRel rels) -> return (SomeRel rels EEP.relSym)
    _ -> Nothing
  let revNameRelaMap = F.foldl' (pltStubAddresses dynSec vam vdefm vreqm getRelSymIdx) [] rels
  let nameRelaMap = zip [0..] (reverse revNameRelaMap)
  pltSec <- listToMaybe (EE.findSectionByName (BSC.pack ".plt") elf)
  let pltBase = EE.elfSectionAddr pltSec
  let (pltSize, pltStubSize) = case EE.elfMachine elf of
        EE.EM_X86_64 -> (16, 16)
        EE.EM_ARM -> (20, 12)
        EE.EM_PPC -> (72, 8)
        em -> error ("Unexpected architecture: " ++ show em)
  return (buildAssocList nameRelaMap (pltSize + toInteger pltBase) pltStubSize loadOptions)

extractPltGotAddrs
  :: forall proxy tp w
   . ( EEP.IsRelocationType tp
     , DMM.MemWidth w
     , w ~ EEP.RelocationWidth tp
     , Integral (EEP.ElfWordType w)
     )
  => proxy tp
  -> EEP.DynamicSection (EEP.RelocationWidth tp)
  -> EEP.VirtAddrMap (EEP.RelocationWidth tp)
  -> EEP.VersionDefMap
  -> EEP.VersionReqMap
  -> DMML.LoadOptions
  -> EE.Elf w
  -> Maybe
  [(DMM.MemWord w,
     EEP.SymtabEntry BSC.ByteString (EEP.ElfWordType (EEP.RelocationWidth tp)))]
extractPltGotAddrs _ dynSec vam vdefm vreqm loadOptions elf = do
  relsGot <- case EEP.dynRelaEntries @tp dynSec vam of
    Right relas -> return relas
    Left _ -> Nothing
  let revNameRelaGotMap = F.foldl' (pltStubAddresses dynSec vam vdefm vreqm EEP.relaSym) [] relsGot
  let nameRelaMapGot = zip [0..] (reverse revNameRelaGotMap)

  pltGotSec <- listToMaybe (EE.findSectionByName (BSC.pack ".plt.got") elf)
  let pltGotBase = EE.elfSectionAddr pltGotSec

  let pltGotStubSize = case EE.elfMachine elf of
        EE.EM_X86_64 -> 8
        em -> error ("Unexpected architecture: " ++ show em)
  return (buildAssocList nameRelaMapGot (toInteger pltGotBase) pltGotStubSize loadOptions)

type family ArchRelocationType arch

-- | Match up names PLT stub entries
--
-- Calls to functions in shared libraries are issued through PLT stubs. These
-- are short sequences included in the binary by the compiler that jump to the
-- *real* function implementation in the shared library via the Global Offset
-- Table.  The GOT is populated by the dynamic loader.
--
-- The name for the PLT stub for the @read@ function will be named @read\@plt@.
--
-- See Note [PLT Stub Names] for details
pltStubSymbols
  :: forall arch env w binFmt reloc
   . ( R.ArchConstraints arch
     , R.HasAnalysisEnv env
     , R.HasSymbolicBlockMap env
     , w ~ DMC.ArchAddrWidth arch
     , binFmt ~ EE.ElfHeaderInfo w
     , ArchRelocationType arch ~ reloc
     , EEP.IsRelocationType reloc
     , EEP.RelocationWidth reloc ~ w
     , Integral (EEP.ElfWordType w)
     )
  => env arch binFmt
  -> Map.Map BS.ByteString (R.ConcreteAddress arch)
pltStubSymbols env = Map.fromList $ fromMaybe [] $ do
  vam <- EEP.virtAddrMap elfBytes phdrs

  -- Parse out the contents of the @.dynamic@ section; there is not a single
  -- convenient combinator to extract these entries, so we have to unpack a few
  -- different tables using some elf-edit helpers
  rawDynSec <- listToMaybe (EE.findSectionByName (BSC.pack ".dynamic") elf)
  let dynBytes = EE.elfSectionData rawDynSec
  dynSec <- case EEP.dynamicEntries (EE.elfData elf) (EE.elfClass elf) dynBytes of
    Left _dynErr -> Nothing
    Right dynSec -> return dynSec
  vdefm <- case EEP.dynVersionDefMap dynSec vam of
    Left _dynErr -> Nothing
    Right vm -> return vm
  vreqm <- case EEP.dynVersionReqMap dynSec vam of
    Left _dynErr -> Nothing
    Right vm -> return vm

  -- PLT symbol names can exist in two different places depending on how they
  -- are referenced; check both and accumulate all of them here.
  let pltAddrs = fromMaybe [] $ extractPltAddrs (Proxy @reloc) dynSec vam vdefm vreqm loadOptions elf
  let pltGotAddrs = fromMaybe [] $ extractPltGotAddrs (Proxy @reloc) dynSec vam vdefm vreqm loadOptions elf

  return [ (EE.steName entry, R.concreteFromAbsolute w)
         | (w, entry) <- pltAddrs ++ pltGotAddrs
         ]
  where
    bin = R.analysisLoadedBinary env
    elfHeaderInfo = MBL.originalBinary bin
    loadOptions = MBL.loadOptions bin
    phdrs = EE.headerPhdrs elfHeaderInfo
    elfBytes = EE.headerFileContents elfHeaderInfo
    (_, elf) = EE.getElf elfHeaderInfo
