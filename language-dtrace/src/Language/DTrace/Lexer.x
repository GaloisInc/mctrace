{

-- | This is a lexer for the DTrace language (referred to as the D language in the OpenDTrace documentation).
--
-- This language is vaguely C-like in syntax, and is used for writing DTrace probes.
module Language.DTrace.Lexer (
    dtraceLexer
  , AlexReturn(..)
  , AlexInput
  , SourcePos(..)
  , SourceRange(..)
  ) where

import           Data.Ratio ( Ratio )
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Language.DTrace.LexerWrapper
import qualified Language.DTrace.Token as DT

}

$digit = 0-9
$hexdigit = [0-9a-fA-F]
$alpha = [a-zA-Z]

tokens :-
  <0> [\"]          { pushStartCode string >> token DT.DQUOTE }
  <string> [\"]     { popStartCode StartCodeError >> token DT.DQUOTE }
  <string> (. # [\"])* { readToken (\t -> return (t, mempty)) DT.STRINGLIT }
  char              { token DT.CHAR }
  short             { token DT.SHORT }
  int               { token DT.INT }
  signed            { token DT.SIGNED }
  unsigned          { token DT.UNSIGNED }
  long              { token DT.LONG }
  userland          { token DT.USERLAND }
  string            { token DT.STRING }
  void              { token DT.VOID }
  float             { token DT.FLOAT }
  double            { token DT.DOUBLE }
  struct            { token DT.STRUCT }
  union             { token DT.UNION }
  enum              { token DT.ENUM }

  self              { token DT.SELF }
  this              { token DT.THIS }
  [\-][\>]          { token DT.ARROW }

  [=][=]            { token DT.EQUALS }
  [\!][=]           { token DT.NOT_EQUALS }
  [\<]              { token DT.LESS_THAN }
  [\<][=]           { token DT.LESS_EQUAL }
  [\>]              { token DT.GREATER_THAN }
  [\>][=]           { token DT.GREATER_EQUAL }
  [=]               { token DT.ASSIGN }
  [\+][=]           { token DT.PLUS_ASSIGN }
  [\-][=]           { token DT.MINUS_ASSIGN }
  [\*][=]           { token DT.TIMES_ASSIGN }
  [\/][=]           { token DT.DIVIDE_ASSIGN }
  [\+][\+]          { token DT.PLUSPLUS }
  [\-][\-]          { token DT.MINUSMINUS }
  [\+]              { token DT.PLUS }
  [\-]              { token DT.MINUS }
  [\*]              { token DT.STAR }
  [\/]              { token DT.SLASH }
  [\%]              { token DT.MODULUS }

  [\!]              { token DT.EXCLAMATION }
  [\~]              { token DT.TILDE }
  [\:]              { token DT.COLON }

  [\&]              { token DT.AMPERSAND }
  [\^]              { token DT.CARET }
  [\|]              { token DT.PIPE }

  [\<][\<]          { token DT.SHIFT_LEFT }
  [\>][\>]          { token DT.SHIFT_RIGHT }

  [\;]              { token DT.SEMI }
  [\(]              { token DT.LPAREN }
  [\)]              { token DT.RPAREN }
  [\{]              { token DT.LBRACE }
  [\}]              { token DT.RBRACE }
  [\[]              { token DT.LBRACKET }
  [\]]              { token DT.RBRACKET }
  [\/]              { token DT.SLASH }
  [\\]              { token DT.BACKSLASH }
  [\,]              { token DT.COMMA }

  $digit+           { readToken TR.decimal (DT.INTLIT DT.Decimal) }
  0x $hexdigit+     { readToken TR.hexadecimal (DT.INTLIT DT.Hexadecimal) }
  $digit+ \. $digit+ { readToken readFloat (uncurry DT.DOUBLELIT) }

  $alpha [$alpha $digit \_]* { readToken (\t -> return (t, mempty)) DT.IDENT }

  <comment> [\/][\/] .+ ;
  <comment> [\n]         { popStartCode StartCodeError >> skip }

  $white+           ;

{

type AlexUserState = ()
alexInitUserState = ()

readFloat :: TR.Reader (T.Text, Double)
readFloat t = do
  (d, t') <- TR.double t
  return ((t, d), t')

readToken :: TR.Reader a
          -> (a -> DT.Token)
          -> Action LexingError AlexUserState (Lexeme DT.Token)
readToken rdr con = readLexeme rdr ErrorAt con

token :: DT.Token -> Action LexingError AlexUserState (Lexeme DT.Token)
token tk = lexeme tk

ratioWithText :: TR.Reader (T.Text, Ratio Integer)
ratioWithText t = do
  (r, rest) <- TR.rational t
  return ((t, r), rest)


alexMonadScan :: Action LexingError AlexUserState (Lexeme DT.Token)
alexMonadScan = do
  inp <- endInput
  sc <- getStartCode StartCodeError
  case alexScan inp sc of
    AlexSkip inp' _len -> do
      setNextInput inp'
      alexMonadScan
    AlexToken inp' len action -> do
      setInputRange inp inp' len
      action
    AlexError inp' -> raise (\_ _ -> ErrorPos (inputPos inp'))
    AlexEOF -> return (Lexeme (T.pack "") DT.EOF (SourceRange endPos endPos))

skip = alexMonadScan

dtraceLexer :: AlexInput -> Int -> AlexReturn (Action LexingError AlexUserState (Lexeme DT.Token))
dtraceLexer = alexScan
}
