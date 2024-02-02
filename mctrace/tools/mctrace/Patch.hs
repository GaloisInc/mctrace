{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
{-# Language ConstraintKinds #-}
{-# Language RankNTypes #-}
{-# Language PolyKinds #-}

module Patch (
    patchElf
) where
    
import qualified Control.Exception as X
import           Control.Monad ( unless )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Functor.Identity as I

import qualified Data.ElfEdit as E

data PatchException = PatchException 
    deriving Show

instance X.Exception PatchException

patchElf :: E.SomeElf E.ElfHeaderInfo -> IO (E.SomeElf E.ElfHeaderInfo)
patchElf (E.SomeElf eHeaderInfo) = do
    -- Parse the elf
    -- TODO: Use a better mechanism than just printing out parse errors
    let (elfParseErrors, elf) = E.getElf eHeaderInfo
    unless (null elfParseErrors) $
        mapM_ print elfParseErrors
    updateElfSections elf upd
    where
        upd sec = patchSectionTypes expectedSectionTypes sec

patchSectionTypes :: Map.Map BS.ByteString E.ElfSectionType -> E.ElfSection w -> Maybe (E.ElfSection w)
patchSectionTypes expectedTypes sec = do
    case Map.lookup (E.elfSectionName sec) expectedTypes of
        Just ty -> Just (sec { E.elfSectionType = ty} )
        Nothing -> Just sec

expectedSectionTypes :: Map.Map BS.ByteString E.ElfSectionType
expectedSectionTypes = 
    Map.fromList [ (".plt", E.SHT_PROGBITS) ]


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
