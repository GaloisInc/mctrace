module Options (
    IOptions(..)
  , EOptions(..)
  , Options(..)
  , options
  ) where

import qualified Options.Applicative as O

-- | Options for the instrumentation command
data IOptions =
  IOptions { iDTraceFile :: FilePath
           -- ^ The DTrace script specifying the probes to insert
           , iInputExecutableFile :: FilePath
           -- ^ The executable file to instrument
           , iOutputExecutableFile :: FilePath
           -- ^ The path to save the resulting modified binary to
           , iVarMappingFile :: FilePath
           -- ^ The file to save the mapping from variable names to their
           -- offsets into the persistence file
           , iPersistenceFile :: FilePath
           -- ^ The file that the instrumented binary should save its collected
           -- probe data
           , iLLVMAsmFile :: Maybe FilePath
           -- ^ An optional file to save the generated LLVM assembly for the
           -- probes to (for debugging purposes)
           , iAsmFile :: Maybe FilePath
           -- ^ An optional file to save the generated native assembly for the
           -- probes to (for debugging purposes)
           , iObjFile :: Maybe FilePath
           -- ^ An optional file to save the generated object file for the
           -- probes to (for debugging purposes)
           }

-- | Options for the data extraction command
data EOptions =
  EOptions { eVarMappingFile :: FilePath
           -- ^ The mapping file from variables to offsets in the persistence
           -- file (generated by the instrumentation command)
           , ePersistenceFile :: FilePath
           -- ^ The binary file produced by running the instrumented binary
           , eExtractOutput :: Maybe FilePath
           -- ^ The path of a JSON file to save the extracted results to
           }

data Options = Instrument IOptions
             | Extract EOptions

options :: O.ParserInfo Options
options = O.info (O.helper <*> O.hsubparser parser)
          ( O.fullDesc
          <> O.progDesc "Instrument a binary with trace points"
          )
  where
    parser = (O.command "instrument" (O.info iparser (O.progDesc "Instrument a binary with a set of DTrace probes")))
             <> (O.command "extract" (O.info eparser (O.progDesc "Extract collected information from a run of an instrumented binary")))
    iparser = Instrument <$> (IOptions <$> O.strOption
                          ( O.long "script"
                          <> O.metavar "FILE"
                          <> O.help "A DTrace script defining the probes to inject into the binary"
                          )
                       <*> O.strOption
                           ( O.long "binary"
                           <> O.metavar "FILE"
                           <> O.help "The binary file to instrument"
                           )
                       <*> O.strOption
                           ( O.long "output"
                           <> O.metavar "FILE"
                           <> O.help "The path to store the instrumented binary file"
                           )
                       <*> O.strOption
                             ( O.long "var-mapping"
                             <> O.metavar "FILE"
                             <> O.help "Save the mapping of variable names to offsets into storage for global variables"
                             )
                       <*> O.strOption
                             ( O.long "persistence-file"
                             <> O.metavar "FILE"
                             <> O.help "The file to which the instrumented binary should persist its global values for offline analysis"
                             )
                       <*> O.optional (O.strOption
                             ( O.long "save-llvm-asm"
                             <> O.metavar "FILE"
                             <> O.help "A file to save generated LLVM assembly to"
                             ))
                       <*> O.optional (O.strOption
                             ( O.long "save-asm"
                             <> O.metavar "FILE"
                             <> O.help "A file to save generated machine assembly to"
                             ))
                       <*> O.optional (O.strOption
                             ( O.long "save-obj"
                             <> O.metavar "FILE"
                             <> O.help "A file to save generated object file to"
                             )))
    eparser = Extract <$> (EOptions <$> O.strOption
                           ( O.long "var-mapping"
                           <> O.metavar "FILE"
                           <> O.help "The mapping file associated with this persisted data (generated by the instrumentation phase)"
                           )
                       <*> O.strOption
                           ( O.long "persistence-file"
                           <> O.metavar "FILE"
                           <> O.help "The state produced by the instrumented binary"
                           )
                       <*> O.optional (O.strOption
                           ( O.long "output"
                           <> O.metavar "FILE"
                           <> O.help "The JSON file to save the extracted probe data from"
                           )))
