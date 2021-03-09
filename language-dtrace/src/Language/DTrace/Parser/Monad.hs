{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.DTrace.Parser.Monad (
    ParseM
  , runParseM
  , initialParseState
  , getStartCodes
  , setStartCodes
  , ParseError(..)
  , ppParseError
  , ParseState(..)
  , parseError
  , withRanges
  ) where

import qualified Control.Exception as X
import qualified Control.Monad.Catch.Pure as CMC
import qualified Control.Monad.State.Strict as CMS
import qualified Data.Text as T

import qualified Language.DTrace.LexerWrapper as DLW

data ParseError = UnexpectedToken !T.Text !DLW.SourceRange
                | LexingError DLW.AlexInput
                | LexingFailure DLW.LexingError
                | MultipleFileSourceRange [DLW.SourceRange]
                | EmptyRangeList
                | EmptyStartCodeStack
  deriving (Show)

instance X.Exception ParseError

ppParseError :: ParseError -> String
ppParseError pe =
  case pe of
    UnexpectedToken nm r -> do
      DLW.prettySourceRangeMsg r $ "Unexpected token " ++ T.unpack nm
    LexingError i ->
      DLW.prettySourcePosMsg (DLW.inputPos i) $ "Syntax error"
    LexingFailure (DLW.ErrorAt msg r _) ->
      DLW.prettySourceRangeMsg r msg
    LexingFailure (DLW.ErrorPos p) -> "Syntax error at " ++ DLW.prettySourcePos p
    LexingFailure (DLW.StartCodeError r _) ->
      DLW.prettySourceRangeMsg r "Start code nesting error"
    -- Note: The two cases below seem like internal implementation errors.
    MultipleFileSourceRange _ ->
      "multiple file source range"
    EmptyRangeList ->
      "empty range list"
    EmptyStartCodeStack -> "Empty start code stack"

data ParseState =
  ParseState { lexerInput :: !DLW.AlexInput
             , currentStartCodes :: [Int]
             }

newtype ParseM a = ParseM { unParseM :: CMC.CatchT (CMS.State ParseState) a }
  deriving ( Functor
           , Applicative
           , Monad
           , CMS.MonadState ParseState
           , CMC.MonadThrow
           )

runParseM :: ParseState -> ParseM a -> Either X.SomeException a
runParseM s0 act = CMS.evalState (CMC.runCatchT (unParseM act)) s0

parseError :: DLW.Lexeme t -> ParseM a
parseError t = CMC.throwM (UnexpectedToken (DLW.lexemeText t) (DLW.lexemeRange t))

initialParseState :: FilePath -> T.Text -> ParseState
initialParseState fp txt =
  ParseState { lexerInput = DLW.initialInput fp txt
             , currentStartCodes = [0]
             }

getStartCodes :: ParseM [Int]
getStartCodes = CMS.gets currentStartCodes

setStartCodes :: [Int] -> ParseM ()
setStartCodes sc = CMS.modify' $ \s -> s { currentStartCodes = sc }

withRanges :: (DLW.HasRange r) => [r] -> (DLW.SourceRange -> a) -> ParseM a
withRanges lexemes = withRanges' (map DLW.range lexemes)

-- | Compute a combined range for a list of input ranges
--
-- Will raise an error if the source ranges are not all from the same file
withRanges' :: [DLW.SourceRange] -> (DLW.SourceRange -> a) -> ParseM a
withRanges' ranges k =
  case ranges of
    [] -> CMC.throwM EmptyRangeList
    r:rs
      | all (\x -> DLW.sourceFile x == DLW.sourceFile (DLW.sourceFrom r)) (concatMap rangePositions rs) ->
        let r' = DLW.SourceRange { DLW.sourceFrom = minimum (DLW.sourceTo r : DLW.sourceFrom r : concatMap rangePositions rs)
                                , DLW.sourceTo = maximum (DLW.sourceTo r : DLW.sourceFrom r : concatMap rangePositions rs)
                                }
        in return (k r')
      | otherwise -> CMC.throwM (MultipleFileSourceRange ranges)

rangePositions :: DLW.SourceRange -> [DLW.SourcePos]
rangePositions r = [DLW.sourceFrom r, DLW.sourceTo r]
