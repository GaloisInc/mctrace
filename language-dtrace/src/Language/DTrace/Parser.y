{
{-# LANGUAGE ViewPatterns #-}
module Language.DTrace.Parser (
  parseDTrace
  ) where

import qualified Control.Exception as X
import qualified Control.Monad.Catch.Pure as CMC
import qualified Control.Monad.State.Strict as CMS
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import           Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import           Numeric.Natural ( Natural )

import qualified Language.DTrace.Lexer as LDL
import qualified Language.DTrace.LexerWrapper as LDLW
import qualified Language.DTrace.Parser.Monad as DM
import qualified Language.DTrace.ProbeDescription as LDP
import qualified Language.DTrace.Syntax.Untyped as DS
import qualified Language.DTrace.Token as DT


}

%name dtraceParser
%tokentype { LDLW.Lexeme DT.Token }
%monad { DM.ParseM }
%lexer { lexer } { LDLW.Lexeme _ DT.EOF _ }
%error { DM.parseError }

%token
  INTLIT { (intLitPos -> Just $$ ) }
  STRINGLIT { (stringLitPos -> Just $$) }
  IDENTIFIER { (identPos -> Just $$) }
  INT { LDLW.Lexeme _ DT.INT _ }
  FLOAT { LDLW.Lexeme _ DT.FLOAT _ }
  DOUBLE { LDLW.Lexeme _ DT.DOUBLE _ }
  ARROW { LDLW.Lexeme _ DT.ARROW _ }
  SELF { LDLW.Lexeme _ DT.SELF _ }
  THIS { LDLW.Lexeme _ DT.THIS _ }

  LOGIC_OR { LDLW.Lexeme _ DT.LOGIC_OR _ }
  LOGIC_AND { LDLW.Lexeme _ DT.LOGIC_AND _ }
  EQUALS { LDLW.Lexeme _ DT.EQUALS _ }
  NOT_EQUALS { LDLW.Lexeme _ DT.NOT_EQUALS _ }
  LESS_THAN { LDLW.Lexeme _ DT.LESS_THAN _ }
  LESS_EQUAL { LDLW.Lexeme _ DT.LESS_EQUAL _ }
  GREATER_THAN { LDLW.Lexeme _ DT.GREATER_THAN _ }
  GREATER_EQUAL { LDLW.Lexeme _ DT.GREATER_EQUAL _ }

  PLUSPLUS { LDLW.Lexeme _ DT.PLUSPLUS _ }
  MINUSMINUS { LDLW.Lexeme _ DT.MINUSMINUS _ }

  PLUS { LDLW.Lexeme _ DT.PLUS _ }
  MINUS { LDLW.Lexeme _ DT.MINUS _ }
  STAR { LDLW.Lexeme _ DT.STAR _ }
  SLASH { LDLW.Lexeme _ DT.SLASH _ }
  MODULUS { LDLW.Lexeme _ DT.MODULUS _ }

  SHIFT_LEFT { LDLW.Lexeme _ DT.SHIFT_LEFT _ }
  SHIFT_RIGHT { LDLW.Lexeme _ DT.SHIFT_RIGHT _ }

  AMPERSAND { LDLW.Lexeme _ DT.AMPERSAND _ }
  PIPE { LDLW.Lexeme _ DT.PIPE _ }
  CARET { LDLW.Lexeme _ DT.CARET _ }
  EXCLAMATION { LDLW.Lexeme _ DT.EXCLAMATION _ }
  TILDE { LDLW.Lexeme _ DT.TILDE _ }
  QUESTION { LDLW.Lexeme _ DT.QUESTION _ }
  ASSIGN { LDLW.Lexeme _ DT.ASSIGN _ }

  LPAREN { LDLW.Lexeme _ DT.LPAREN _ }
  RPAREN { LDLW.Lexeme _ DT.RPAREN _ }
  LBRACE { LDLW.Lexeme _ DT.LBRACE _ }
  RBRACE { LDLW.Lexeme _ DT.RBRACE _ }
  
  COMMA { LDLW.Lexeme _ DT.COMMA _ }
  SEMI { LDLW.Lexeme _ DT.SEMI _ }
  COLON { LDLW.Lexeme _ DT.COLON _ }
  
%%

TopLevels :: { [LDLW.Located DS.TopLevel] }
TopLevels : ReverseTopLevels { reverse $1 }

Expr :: { LDLW.Located DS.Expr }
Expr : Assignment { $1 }

Assignment :: { LDLW.Located DS.Expr }
Assignment : Ternary ASSIGN Assignment {% mkE [LDLW.range $1, LDLW.range $3] (DS.Assign $1 $3) }
           | Ternary { $1 }

Ternary :: { LDLW.Located DS.Expr }
Ternary : LogicalOr QUESTION LogicalOr COLON Ternary {% mkE [LDLW.range $1, LDLW.range $5] (DS.Ternary $1 $3 $5) }
        | LogicalOr { $1 }

LogicalOr :: { LDLW.Located DS.Expr }
LogicalOr : LogicalOr LOGIC_OR LogicalAnd {% mkE [LDLW.range $1, LDLW.range $3] (DS.Or $1 $3) }
          | LogicalAnd { $1 }

LogicalAnd :: { LDLW.Located DS.Expr }
LogicalAnd : LogicalAnd LOGIC_AND RelEq {% mkE [LDLW.range $1, LDLW.range $3] (DS.And $1 $3) }
           | RelEq { $1 }

RelEq :: { LDLW.Located DS.Expr }
RelEq : RelEq EQUALS RelCompare {% mkE [LDLW.range $1, LDLW.range $3] (DS.Eq $1 $3) }
      | RelEq NOT_EQUALS RelCompare {% mkE [LDLW.range $1, LDLW.range $3] (DS.Ne $1 $3) }
      | RelCompare { $1 }

RelCompare :: { LDLW.Located DS.Expr }
RelCompare : RelCompare LESS_THAN Shift {% mkE [LDLW.range $1, LDLW.range $3] (DS.Lt $1 $3) }
           | RelCompare LESS_EQUAL Shift {% mkE [LDLW.range $1, LDLW.range $3] (DS.Le $1 $3) }
           | RelCompare GREATER_THAN Shift {% mkE [LDLW.range $1, LDLW.range $3] (DS.Gt $1 $3) }
           | RelCompare GREATER_EQUAL Shift {% mkE [LDLW.range $1, LDLW.range $3] (DS.Ge $1 $3) }
           | Shift { $1 }

Shift :: { LDLW.Located DS.Expr }
Shift : Shift SHIFT_LEFT Additive {% mkE [LDLW.range $1, LDLW.range $3] (DS.BVShl $1 $3) }
      | Shift SHIFT_RIGHT Additive {% mkE [LDLW.range $1, LDLW.range $3] (DS.BVLshr $1 $3) }
      | Additive { $1 }

Additive :: { LDLW.Located DS.Expr }
Additive : Additive PLUS Term {% mkE [LDLW.range $1, LDLW.range $3] (DS.Add $1 $3) }
         | Additive MINUS Term {% mkE [LDLW.range $1, LDLW.range $3] (DS.Sub $1 $3) }
         | Term { $1 }

Term :: { LDLW.Located DS.Expr }
Term : Term STAR Unary {% mkE [LDLW.range $1, LDLW.range $3] (DS.Mul $1 $3) }
     | Term SLASH Unary {% mkE [LDLW.range $1, LDLW.range $3] (DS.Div $1 $3) }
     | Term MODULUS Unary {% mkE [LDLW.range $1, LDLW.range $3] (DS.Mod $1 $3) }
     | Unary { $1 }

Unary :: { LDLW.Located DS.Expr }
Unary : MINUS Factor {% mkE [LDLW.range $1, LDLW.range $2] (DS.Neg $2) }
      | PLUS Factor { $2 }
      | EXCLAMATION Factor {% mkE [LDLW.range $1, LDLW.range $2] (DS.Not $2) }
      | TILDE Factor {% mkE [LDLW.range $1, LDLW.range $2] (DS.BVNeg $2) }
      | LPAREN Type RPAREN Factor {% mkE [LDLW.range $1, LDLW.range $4] (DS.Cast $2 $4) }
      | Factor { $1 }

ReverseArguments :: { [LDLW.Located DS.Expr] }
ReverseArguments :                             { [] }
                 | Expr                        { [$1] }
                 | ReverseArguments COMMA Expr { $3 : $1 }

Arguments :: { [LDLW.Located DS.Expr] }
Arguments : ReverseArguments { reverse $1 }

Factor :: { LDLW.Located DS.Expr }
Factor : Literal { $1 }
       | IDENTIFIER {% mkE [fst $1] (DS.VarRef (snd $1)) }
       | LPAREN Expr RPAREN { $2 }
       | IDENTIFIER LPAREN Arguments RPAREN {% mkE [fst $1, $4] (DS.Call (snd $1) $3) }
       | THIS {% mkE [$1] DS.This }
       | SELF {% mkE [$1] DS.Self }


Literal :: { LDLW.Located DS.Expr }
Literal : INTLIT {% mkE [intLitToken $1] (DS.LitInt (intLitFormat $1) (intLitValue $1)) }
        | STRINGLIT {% mkE [fst $1] (DS.LitString (snd $1)) }

Type :: { LDLW.Located DS.Type }
Type : INT {% mkT [$1] DS.IntTy }
     | DOUBLE {% mkT [$1] DS.DoubleTy }
     | FLOAT {% mkT [$1] DS.FloatTy }

Stmt :: { LDLW.Located DS.Stmt }
Stmt : Expr SEMI {% mkS [$1] (DS.ExprStmt $1) }
     | Type IDENTIFIER SEMI {% mkS [LDLW.range $1, LDLW.range $3] (DS.DeclStmt $1 (snd $2)) }

ReverseStmts :: { [LDLW.Located DS.Stmt] }
ReverseStmts :                       { [] }
             | Stmt                  { [$1] }
             | ReverseStmts Stmt     { $2 : $1 }

Stmts :: { [LDLW.Located DS.Stmt] }
Stmts : ReverseStmts { reverse $1 }

-- FIXME: This needs to account for patterns
--
-- FIXME: These are actually a superset of identifiers that can contain dashes
MaybeProbeComponent :: { Maybe (LDLW.Lexeme DT.Token, T.Text) }
MaybeProbeComponent : IDENTIFIER { Just $1 }
                    |                { Nothing }

ProbeDescription :: { LDLW.Located LDP.ProbeDescription }
ProbeDescription : MaybeProbeComponent COLON MaybeProbeComponent COLON MaybeProbeComponent COLON MaybeProbeComponent
                    {% mkLocated [LDLW.range (firstToken (fmap fst $1) $2), LDLW.range (firstToken (fmap fst $7) $6)]
                       (LDP.ProbeDescription (asProbe $1) (asProbe $3) (asProbe $5) (asProbe $7)) }

ReverseProbeDescriptions :: { DLN.NonEmpty (LDLW.Located LDP.ProbeDescription) }
ReverseProbeDescriptions : ProbeDescription   { $1 DLN.:| [] }
                         | ReverseProbeDescriptions COMMA ProbeDescription { neCons $3 $1 }

ProbeDescriptions :: { DLN.NonEmpty (LDLW.Located LDP.ProbeDescription) }
ProbeDescriptions : ReverseProbeDescriptions { DLN.reverse $1 }

Probe :: { LDLW.Located DS.Probe }
Probe : ProbeDescriptions LBRACE Stmts RBRACE
        {% mkLocated [LDLW.range (DLN.head $1), LDLW.range $4] (DS.Probe $1 $3) }

TopLevel :: { LDLW.Located DS.TopLevel }
TopLevel : Probe {% mkLocated [LDLW.range $1] (DS.TopProbe $1) }
         | Type IDENTIFIER SEMI {% mkLocated [LDLW.range $1, LDLW.range $3] (DS.TopDecl $1 (snd $2)) }

ReverseTopLevels :: { [LDLW.Located DS.TopLevel] }
ReverseTopLevels :                           { [] }
                 | TopLevel                  { [$1] }
                 | ReverseTopLevels TopLevel { $2 : $1 }

{

asProbe = maybe mempty snd

firstToken :: Maybe (LDLW.Lexeme DT.Token) -> LDLW.Lexeme DT.Token -> LDLW.Lexeme DT.Token
firstToken mt t = fromMaybe t mt

neCons :: a -> DLN.NonEmpty a -> DLN.NonEmpty a
neCons a1 (a2 DLN.:| as) = a1 DLN.:| (a2 : as)

mkLocated :: (LDLW.HasRange r) => [r] -> a -> DM.ParseM (LDLW.Located a)
mkLocated tokens l = DM.withRanges tokens (\r -> LDLW.Located r l)

mkS :: (LDLW.HasRange r) => [r] -> DS.Stmt -> DM.ParseM (LDLW.Located DS.Stmt)
mkS tokens s = DM.withRanges tokens (\r -> LDLW.Located r s)

mkT :: (LDLW.HasRange r) => [r] -> DS.Type -> DM.ParseM (LDLW.Located DS.Type)
mkT tokens t = DM.withRanges tokens (\r -> LDLW.Located r t)

mkE :: (LDLW.HasRange r) => [r] -> DS.App (LDLW.Located DS.Expr) -> DM.ParseM (LDLW.Located DS.Expr)
mkE tokens app = DM.withRanges tokens (\r -> LDLW.Located r (DS.Expr app))

data IntLit = IntLit { intLitToken :: LDLW.Lexeme DT.Token
                     , intLitFormat :: DT.NumericLiteralFormat
                     , intLitValue :: Natural
                     }

intLitPos :: LDLW.Lexeme DT.Token -> Maybe IntLit
intLitPos l =
  case LDLW.lexemeToken l of
    DT.INTLIT litFmt n -> Just (IntLit l litFmt n)
    _ -> Nothing

stringLitPos :: LDLW.Lexeme DT.Token -> Maybe (LDLW.Lexeme DT.Token, T.Text)
stringLitPos l =
  case LDLW.lexemeToken l of
    DT.STRINGLIT t -> Just (l, t)
    _ -> Nothing

identPos :: LDLW.Lexeme DT.Token -> Maybe (LDLW.Lexeme DT.Token, T.Text)
identPos l =
  case LDLW.lexemeToken l of
    DT.IDENT t -> Just (l, t)
    _ -> Nothing

lexer :: (LDLW.Lexeme DT.Token -> DM.ParseM a)
       -> DM.ParseM a
lexer k = do
  a0 <- CMS.gets DM.lexerInput
  scs <- DM.getStartCodes
  case scs of
    [] -> CMC.throwM DM.EmptyStartCodeStack
    sc0 : _ ->
      case LDL.dtraceLexer a0 sc0 of
        LDL.AlexEOF -> k (LDLW.Lexeme (T.pack "") DT.EOF (LDLW.range (LDLW.inputPos a0)))
        LDL.AlexError rest ->
          CMC.throwM (DM.LexingError rest)
        LDL.AlexSkip next _skippedLen -> do
          CMS.modify' $ \s -> s { DM.lexerInput = next }
          lexer k
        LDL.AlexToken next tokenLen action -> do
            -- NOTE: The action may advance the stream by skipping tokens.  We need to capture the updated
            -- next token after that
          case LDLW.runAction action a0 next tokenLen () scs of
            Left err -> CMC.throwM (DM.LexingFailure err)
            Right (tok, nextState) -> do
              CMS.modify' $ \s -> s { DM.lexerInput = LDLW.matchingInputEnd nextState }
              DM.setStartCodes (LDLW.startCode nextState)
              k tok

parseDTrace :: FilePath -> T.Text -> Either X.SomeException [LDLW.Located DS.TopLevel]
parseDTrace filePath txt = DM.runParseM (DM.initialParseState filePath txt) dtraceParser

}
