{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
{-# Language ConstraintKinds #-}
{-# Language RankNTypes #-}
{-# Language PolyKinds #-}
{-# Language FlexibleContexts #-}
{-# Language TypeApplications #-}
{-# Language ScopedTypeVariables #-}

module Patch
  ( ElfPatch(..)
  , patchElf
  )
where

import qualified Control.Exception as X
import           Control.Monad ( unless, forM_ )
import           Data.Bits ( (.|.) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import           Data.Maybe ( catMaybes )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Functor.Identity as I

import qualified Data.ElfEdit as E

data PatchException = PatchException
    deriving Show

instance X.Exception PatchException

-- | The patches that we'd like to apply.
--
-- Some binaries do not satisfy various assumptions made by one or more
-- libraries used by this tool. These patches give us a way to modify
-- the incoming binary before it hits the underyling libraries, so if
-- we know that a binary needs to be modified to prevent a later crash
-- or issue, these patches give us ways to do that. These are provided
-- only to get the incoming binaries to cooperate with this tool; they
-- are not principled in general, and are not at all guaranteed to make
-- sense or even lead to correct tool results. They are provided only as
-- a coping strategy.
data ElfPatch =
    GOTSectionFlags
    -- ^ Set the expected Write/Alloc flags on the .got section and
    -- clear all others.
    | PLTTypeProgbits
    -- ^ Set the .plt section's type to PROGBITS.
    deriving (Eq, Show, Ord)

patchElf :: [ElfPatch] -> E.SomeElf E.ElfHeaderInfo -> IO (E.SomeElf E.ElfHeaderInfo)
patchElf [] e = do
    putStrLn "Notice: no ELF patches enabled"
    return e
patchElf patches (E.SomeElf (eHeaderInfo :: E.ElfHeaderInfo w)) = do
    -- Parse the elf
    -- TODO: Use a better mechanism than just printing out parse errors
    let (elfParseErrors, elf) = E.getElf eHeaderInfo
    unless (null elfParseErrors) $
        mapM_ print elfParseErrors

    forM_ patches $ \p ->
        putStrLn $ "ELF patch enabled: " <> show p

    E.elfClassInstances (E.headerClass $ E.header eHeaderInfo) $ do
        updateElfSections elf (upd (Proxy @w))

    where
        upd :: (E.ElfWidthConstraints w)
            => Proxy w
            -> E.ElfSection (E.ElfWordType w)
            -> Maybe (E.ElfSection (E.ElfWordType w))
        upd p sec = patchSectionTypes (expectedSectionTypes patches) =<<
                    patchSectionFlags (expectedSectionFlags p patches) sec

patchSectionTypes :: Map.Map BS.ByteString E.ElfSectionType
                  -> E.ElfSection w -> Maybe (E.ElfSection w)
patchSectionTypes expectedTypes sec = do
    case Map.lookup (E.elfSectionName sec) expectedTypes of
        Just ty -> Just (sec { E.elfSectionType = ty} )
        Nothing -> Just sec

expectedSectionTypes :: [ElfPatch] -> Map.Map BS.ByteString E.ElfSectionType
expectedSectionTypes patches =
    Map.fromList $ catMaybes $
        [ if PLTTypeProgbits `elem` patches
          then Just (".plt", E.SHT_PROGBITS)
          else Nothing
        ]

patchSectionFlags :: (E.ElfSection w -> Map.Map BS.ByteString (E.ElfSectionFlags w))
                  -> E.ElfSection w -> Maybe (E.ElfSection w)
patchSectionFlags mkExpectedFlags sec = do
    case Map.lookup (E.elfSectionName sec) (mkExpectedFlags sec) of
        Just fs -> Just (sec { E.elfSectionFlags = fs })
        Nothing -> Just sec

expectedSectionFlags :: (E.ElfWidthConstraints w)
                     => Proxy w
                     -> [ElfPatch]
                     -> E.ElfSection (E.ElfWordType w)
                     -> Map.Map BS.ByteString (E.ElfSectionFlags (E.ElfWordType w))
expectedSectionFlags _ patches _ =
    Map.fromList $ catMaybes $
        [ if GOTSectionFlags `elem` patches
          then Just (".got", E.shf_write .|. E.shf_alloc)
          else Nothing
        ]

updateElfSections :: E.Elf w -> (E.ElfSection (E.ElfWordType w) -> Maybe (E.ElfSection (E.ElfWordType w))) -> IO (E.SomeElf E.ElfHeaderInfo)
updateElfSections elf upd = do
    -- Run the update function
    let newElf = I.runIdentity $ E.updateSections upd' elf
    -- Convert back to bytes and then read it back to get a header info instance
    let newElfBytes = BSL.toStrict (E.renderElf newElf)
    case E.decodeElfHeaderInfo newElfBytes of
        Left (_, _) -> X.throwIO PatchException
        Right se -> return se
    where
        upd' sec = pure (upd sec)
