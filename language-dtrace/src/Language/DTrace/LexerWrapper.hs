{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.DTrace.LexerWrapper (
    AlexInput
  , initialInput
  , inputPos
  , endPos
  , alexGetByte
  , alexInputPrevChar
  , SourcePos(..)
  , prettySourcePos
  , prettySourcePosMsg
  , SourceRange(..)
  , prettySourceRange
  , prettySourceRangeMsg
  , HasRange(..)
  , Located(..)
  , Lexeme(..)
  , lexeme
  , readLexeme
  , getStartCode
  , pushStartCode
  , popStartCode
  , setNextInput
  , setInputRange
  , endInput
  , raise
  , Action
  , runAction
  , AlexState(..)
  , LexingError(..)
  , dummyLoc
  ) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Control.Monad.Except as CME
import qualified Control.Monad.State.Strict as CMS
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Prettyprinter as PP
import           Data.Word ( Word8 )

data SourcePos = SourcePos
  { sourceIndex   :: {-# UNPACK #-} !Int
  , sourceLine    :: {-# UNPACK #-} !Int
  , sourceColumn  :: {-# UNPACK #-} !Int
  , sourceFile    :: FilePath
  }
  deriving (Show)

instance Ord SourcePos where
  compare p1 p2 =
    case compare (sourceFile p1) (sourceFile p2) of
      EQ -> compare (sourceIndex p1) (sourceIndex p2)
      o -> o

-- do the same short-circuiting on sourceIndex in Eq as in Ord
instance Eq SourcePos where
  p1 == p2 = compare p1 p2 == EQ


data AlexInput = Input
  { inputPos       :: !SourcePos
  , inputText      :: !T.Text
  , inputCharBytes :: [Word8]
  , inputPrev      :: !SourcePos
  , inputPrevChar  :: {-# UNPACK #-} !Char
  }
  deriving (Show)

-- | Prepare the text for lexing.
initialInput
  :: FilePath
  -- ^ The file where the text came from
  -> T.Text
  -- ^ The text to lex
  -> AlexInput
initialInput file str = Input
  { inputPos      = startPos file
  , inputPrev     = beforeStartPos file
  , inputPrevChar = '\n'    -- end of the virtual previous line
  , inputText     = str
  , inputCharBytes = []
  }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = inputPrevChar

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp0 =
  case inputCharBytes inp0 of
    [] -> do
      (c,text') <- T.uncons (inputText inp0)
      let p'  = moveSourcePos c (inputPos inp0)
          -- This decode is total, as there is always at least one byte for each Char
          x : xs = UTF8.encodeChar c
          inp = Input { inputPrev      = inputPos inp0
                      , inputPrevChar  = c
                      , inputPos       = p'
                      , inputText      = text'
                      , inputCharBytes = xs
                      }
      x `seq` inp `seq` return (x, inp)
    b:bs ->
      let inp1 = Input { inputPrev      = inputPrev inp0
                       , inputPrevChar  = inputPrevChar inp0
                       , inputPos       = inputPos inp0
                       , inputText      = inputText inp0
                       , inputCharBytes = bs
                       }
      in b `seq` inp1 `seq` return (b, inp1)

-- | Update a 'SourcePos' for a particular matched character
moveSourcePos :: Char -> SourcePos -> SourcePos
moveSourcePos c p =
  SourcePos { sourceIndex  = sourceIndex p + 1
            , sourceLine   = newLine
            , sourceColumn = newColumn
            , sourceFile   = sourceFile p
            }
  where
    line   = sourceLine p
    column = sourceColumn p

    (newLine,newColumn) = case c of
                            '\t' -> (line, ((column + 7) `div` 8) * 8 + 1)
                            '\n' -> (line + 1, 1)
                            _    -> (line, column + 1)

startPos :: FilePath -> SourcePos
startPos file =
  SourcePos { sourceIndex   = 0
            , sourceLine    = 1
            , sourceColumn  = 1
            , sourceFile    = file
            }

beforeStartPos :: FilePath -> SourcePos
beforeStartPos file =
  SourcePos { sourceIndex   = -1
            , sourceLine    = 0
            , sourceColumn  = 0
            , sourceFile    = file
            }

-- | Pretty print the source position without the file name.
prettySourcePos :: SourcePos -> String
prettySourcePos x =
  concat [ show (sourceLine x)
         , ":"
         , show (sourceColumn x)
         ]

-- | Pretty print the source position, including the file name.
prettySourcePosLong :: SourcePos -> String
prettySourcePosLong x =
  concat [sourceFile x
         , ":"
         , show (sourceLine x)
         , ":"
         , show (sourceColumn x)
         ]

prettySourcePosMsg :: SourcePos -> String -> String
prettySourcePosMsg r m =
  prettySourcePosLong r ++ ": " ++ m

endPos :: SourcePos
endPos = SourcePos { sourceIndex = -2
                   , sourceLine = -1
                   , sourceColumn = -1
                   , sourceFile = ""
                   }
-- Ranges

data SourceRange = SourceRange
  { sourceFrom :: !SourcePos
  , sourceTo   :: !SourcePos
  } deriving (Show, Eq, Ord)

-- | Pretty print the range, without the file name
prettySourceRange :: SourceRange -> String
prettySourceRange x = prettySourcePos (sourceFrom x) ++ "--" ++
                      prettySourcePos (sourceTo x)

-- | Pretty print the range, including the file name.
prettySourceRangeLong :: SourceRange -> String
prettySourceRangeLong x
  | sourceFile pfrom == sourceFile pto =
    sourceFile pfrom ++ ":" ++
    prettySourcePos pfrom ++ "--" ++
    prettySourcePos pto
  | otherwise = prettySourcePosLong pfrom ++ "--" ++
                prettySourcePosLong pto
  where
  pfrom = sourceFrom x
  pto   = sourceTo x

prettySourceRangeMsg :: SourceRange -> String -> String
prettySourceRangeMsg r m =
  prettySourceRangeLong r ++ ": " ++ m

-- instance NFData SourceRange where
--   rnf (SourceRange x y) = rnf (x,y)

class HasRange t where
  range :: t -> SourceRange

instance HasRange SourcePos where
  range p = SourceRange { sourceFrom = p
                        , sourceTo   = p }

instance HasRange SourceRange where
  range = id

instance (HasRange a, HasRange b) => HasRange (Either a b) where
  range (Left x)  = range x
  range (Right x) = range x

(<->) :: (HasRange a, HasRange b) => a -> b -> SourceRange
x <-> y = SourceRange { sourceFrom = sourceFrom (range x)
                      , sourceTo   = sourceTo   (range y)
                      }

-- Located

-- | Some value along with associated source range information.
data Located a = Located { sourceRange :: !SourceRange
                         , value :: !a
                         }
  deriving (Show)

instance Functor Located where
  fmap f (Located r v) = Located r (f v)

instance (PP.Pretty a) => PP.Pretty (Located a) where
  pretty lv = PP.pretty (value lv)


instance HasRange (Located a) where
  range = sourceRange

dummyLoc :: a -> Located a
dummyLoc x = Located (SourceRange endPos endPos) x

-- Lexeme

data Lexeme t = Lexeme
  { lexemeText  :: !T.Text
  , lexemeToken :: !t
  , lexemeRange :: !SourceRange
  } deriving (Show, Eq)

instance HasRange (Lexeme t) where
  range = lexemeRange

-- instance NFData t => NFData (Lexeme t) where
--   rnf (Lexeme x y z) = rnf (x,y,z)

-- Actions

-- | An action to be taken when a regular expression matches
--
-- Actions are monadic.  The type parameters are:
--
-- - @e@: the type of errors returned when lexing failes
-- - @s@: the type of the user-provided state
newtype Action e s a = A { runA :: CMS.StateT (AlexState s) (CME.Except e) a }
  deriving ( Functor
           , Applicative
           , Monad
           , CMS.MonadState (AlexState s)
           , CME.MonadError e
           )

runAction :: Action e s a
          -> AlexInput
          -> AlexInput
          -> Int
          -> s
          -> [Int]
          -> Either e (a, AlexState s)
runAction a i1 i2 len st scs =
  CME.runExcept (CMS.runStateT (runA a) s0)
  where
    s0 = AlexState { matchingInputStart = i1
                   , matchingInputEnd = i2
                   , matchingInputLength = len
                   , userState = st
                   , startCode = scs
                   }

data AlexState s =
  AlexState { matchingInputStart :: !AlexInput
            , matchingInputEnd :: !AlexInput
            , matchingInputLength :: !Int
            , userState :: !s
            , startCode :: [Int]
            -- ^ A stack of start codes (to facilitate lexing string literals)
            }

raise :: (SourceRange -> T.Text -> e)
      -> Action e s a
raise con = do
  r <- matchRange
  txt <- matchText
  CME.throwError (con r txt)

-- | Access the input just before the regular expression started matching.
startInput :: Action e s AlexInput
startInput = CMS.gets matchingInputStart

-- | Access the input just after the regular expression that matched.
endInput :: Action e s AlexInput
endInput = CMS.gets matchingInputEnd

-- | This is abusing the structure of the state to be able to implement @skip@
-- in @alexMonadScan@.  We set the "next" input to be the end, since that
-- function works by looking at where it last stopped scanning.
setNextInput :: AlexInput -> Action e s ()
setNextInput nextInput =
  CMS.modify' $ \s -> s { matchingInputEnd = nextInput
                       }

setInputRange :: AlexInput -> AlexInput -> Int -> Action e s ()
setInputRange i1 i2 len =
  CMS.modify' $ \s -> s { matchingInputStart = i1
                       , matchingInputEnd = i2
                       , matchingInputLength = len
                       }

-- | The number of characters in the matching input.
matchLength :: Action e s Int
matchLength = CMS.gets matchingInputLength

getStartCode :: (SourceRange -> T.Text -> e) -> Action e s Int
getStartCode onError = do
  cs <- CMS.gets startCode
  case cs of
    [] -> raise onError
    sc : _ -> return sc

pushStartCode :: Int -> Action e s ()
pushStartCode sc =
  CMS.modify' $ \s -> s { startCode = sc : startCode s }

popStartCode :: (SourceRange -> T.Text -> e) -> Action e s ()
popStartCode onError = do
  cs <- CMS.gets startCode
  case cs of
    [] -> raise onError
    _ : cs' -> CMS.modify' $ \s -> s { startCode = cs' }


-- | Get the range for the matching input.
matchRange :: Action e s SourceRange
matchRange =
  do i1 <- startInput
     i2 <- endInput
     return (inputPos i1 <-> inputPrev i2)

-- | Get the text associated with the matched input.
matchText :: Action e s T.Text
matchText =
  do i1 <- startInput
     n  <- matchLength
     return (T.take n (inputText i1))

-- | Use the token and the current match to construct a lexeme.
lexeme :: t -> Action e s (Lexeme t)
lexeme tok =
  do r   <- matchRange
     txt <- matchText
     let l = Lexeme { lexemeRange = r
                    , lexemeToken = tok
                    , lexemeText  = txt
                    }
     l `seq` return l

readLexeme :: TR.Reader a
           -> (String -> SourceRange -> T.Text -> e)
           -> (a -> b)
           -> Action e s (Lexeme b)
readLexeme rdr errCon con = do
  r <- matchRange
  txt <- matchText
  case rdr txt of
    Left err -> raise (errCon err)
    Right (v, _) -> do
      let l = Lexeme { lexemeRange = r
                     , lexemeToken = con v
                     , lexemeText = txt
                     }
      l `seq` return l


data LexingError = ErrorAt String SourceRange T.Text
                 | ErrorPos SourcePos
                 | StartCodeError SourceRange T.Text
  deriving (Show)
