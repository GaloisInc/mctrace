module Language.DTrace.Token (
  NumericLiteralFormat(..),
  Token(..)
  ) where

import qualified Data.Text as T
import           Numeric.Natural ( Natural )

data NumericLiteralFormat = Decimal
                          | Hexadecimal
                          | Octal
                          deriving (Show, Eq, Ord)

data Token = STRINGLIT T.Text
           | INTLIT NumericLiteralFormat Natural
           | FLOATLIT T.Text Float
           | DOUBLELIT T.Text Double
           | IDENT T.Text
           | CHAR
           | SHORT
           | INT
           | SIGNED
           | UNSIGNED
           | LONG
           | USERLAND
           | STRING
           | VOID
           | FLOAT
           | DOUBLE
           | STRUCT
           | UNION
           | ENUM
           | CONST
           | VOLATILE
           | TYPEDEF
           | REGISTER
           | RESTRICT
           | STATIC
           | EXTERN
           | TRANSLATOR
           | THIS
           | SELF
           | XLATE
           | EQUALS
           | NOT_EQUALS
           | LESS_THAN
           | LESS_EQUAL
           | GREATER_THAN
           | GREATER_EQUAL
           | ASSIGN
           | AND_ASSIGN
           | OR_ASSIGN
           | XOR_ASSIGN
           | NOT_ASSIGN
           | MINUS_ASSIGN
           | PLUS_ASSIGN
           | TIMES_ASSIGN
           | DIVIDE_ASSIGN
           | MOD_ASSIGN
           | QUESTION
           | ASTERISK
           | EXCLAMATION
           | INCREMENT
           | DECREMENT
           | TILDE
           | PLUS
           | PLUSPLUS
           | MINUS
           | MINUSMINUS
           | STAR
           | MODULUS
           | AMPERSAND
           | PIPE
           | CARET
           | LOGIC_OR
           | LOGIC_AND
           | SHIFT_LEFT
           | SHIFT_RIGHT
           | COLON
           | LPAREN
           | RPAREN
           | LBRACE
           | RBRACE
           | LBRACKET
           | RBRACKET
           | LANGLE
           | RANGLE
           | COMMA
           | DQUOTE
           | ARROW
           | SLASH
           | BACKSLASH
           | AT
           | SEMI
           | EOF
           deriving (Show)
