module MCTrace.Loader (
    loadBinary
  , loadGeneratedProbes
  , loadProbes
  ) where

import qualified Control.Exception as X
import qualified Data.ByteString as BS
import qualified Data.ElfEdit as DE
import qualified Data.Text.IO as TIO

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT
import qualified MCTrace.Exceptions as ME

-- | Load the input executable from the filesystem and decode its ELF header
--
-- This can throw exceptions if:
--
-- * The file does not exist
--
-- * There is an error decoding the ELF header.
loadBinary :: FilePath -> IO (DE.SomeElf DE.ElfHeaderInfo)
loadBinary exeFile = do
  binBytes <- BS.readFile exeFile
  case DE.decodeElfHeaderInfo binBytes of
    Left (off, msg) -> X.throwIO (ME.ELFDecodeError off msg)
    Right se -> return se

-- | Load the machine code generated from the compiled probes as an ELF file
--
-- This can throw an exception if the generated code is not a recognized ELF file.
loadGeneratedProbes :: BS.ByteString -> IO (DE.SomeElf DE.ElfHeaderInfo)
loadGeneratedProbes bytes =
  case DE.decodeElfHeaderInfo bytes of
    Left (_off, msg) -> X.throwIO (ME.ErrorLoadingGeneratedProbes msg)
    Right se -> return se

-- | Load the given @scriptFile@ as a DTrace script, returning the list of
-- implied probes (which have been type checked)
--
-- This can throw errors if:
--
-- * The file does not exist
--
-- * The file is syntactically not a DTrace script
--
-- * The script fails to type check
loadProbes :: FilePath -> IO LDT.Probes
loadProbes scriptFile = do
  scriptText <- TIO.readFile scriptFile
  case LD.parseDTrace scriptFile scriptText of
    Left err -> X.throwIO (ME.DTraceParseFailure scriptFile err)
    Right decls ->
      case LD.typeCheck decls of
        Left errs -> X.throwIO (ME.DTraceTypeCheckFailure scriptFile errs)
        Right probes -> return probes
