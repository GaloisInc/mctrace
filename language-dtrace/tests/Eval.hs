{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Eval ( mkEvalTest ) where

import           Control.Applicative ( (<|>), empty )
import qualified Data.BitVector.Sized as DBS
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as PN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Void ( Void )
import           Numeric.Natural ( Natural )
import qualified System.FilePath as SF
import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as TTH
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL

import qualified Language.DTrace as LD
import qualified Language.DTrace.Syntax.Typed as LDT
import qualified Language.DTrace.Syntax.Typed.Eval as LDTE

data SomeReg where
  SomeReg :: LDT.Repr tp -> LDTE.RegEntry tp -> SomeReg

-- | This describes the expected result from running the given sequence of
-- probes against an empty initial probe state
--
-- Note that the existential and data family stuff in 'LDTE.RegEntry' means that
-- we have to write our own parser for this (see below)
data EvalExpected =
  EvalExpected { postState :: [(String, SomeReg)]
               , probeSeq :: [LD.ProbeDescription]
               }

type Parser = TM.Parsec Void T.Text

space :: Parser ()
space = TMCL.space TMC.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = TMCL.lexeme space

string :: String -> Parser T.Text
string = TMC.string . T.pack

parseRegBV :: Parser SomeReg
parseRegBV = do
  nBits :: Natural
        <- lexeme (string "RegBV" >> TM.between (TMC.char '<') (TMC.char '>') TMCL.decimal)
  val :: Natural
      <- lexeme TMCL.decimal
  Some nr <- return (PN.mkNatRepr nBits)
  return (SomeReg (LDT.BVRepr nr) (LDTE.RegBV (DBS.mkBV nr (toInteger val))))

parseRegBool :: Parser SomeReg
parseRegBool = do
  _ <- lexeme (string "RegBool")
  b <- TM.try (True <$ lexeme (string "True")) <|> (False <$ lexeme (string "False"))
  return (SomeReg LDT.BoolRepr (LDTE.RegBool b))

parseTraceValue :: Parser SomeReg
parseTraceValue =
  TM.try parseRegBV <|> parseRegBool


parseStateValue :: Parser (String, SomeReg)
parseStateValue = do
  _ <- lexeme (TMC.char '(')
  varName <- TMC.char '"' >> TM.manyTill TMCL.charLiteral (TMC.char '"')
  _ <- lexeme (TMC.char ',')
  val <- parseTraceValue
  _ <- lexeme (TMC.char ')')
  return (varName, val)

parseProbeComponent :: Parser T.Text
parseProbeComponent = T.pack <$> TM.many (TMC.alphaNumChar <|> TMC.char '_')

parseProbeDescription :: Parser LD.ProbeDescription
parseProbeDescription = do
  c1 <- parseProbeComponent
  _ <- TMC.char ':'
  c2 <- parseProbeComponent
  _ <- TMC.char ':'
  c3 <- parseProbeComponent
  _ <- TMC.char ':'
  c4 <- parseProbeComponent
  return LD.ProbeDescription { LD.probeProvider = c1
                             , LD.probeModule = c2
                             , LD.probeFunction = c3
                             , LD.probeName = c4
                             }

parseEvalExpected :: Parser EvalExpected
parseEvalExpected = do
  _ <- lexeme (string "EvalExpected")
  _ <- lexeme (TMC.char '{')
  _ <- lexeme (string "postState")
  _ <- lexeme (TMC.char '=')
  _ <- lexeme (TMC.char '[')
  vals <- parseStateValue `TM.sepBy` lexeme (TMC.char ',')
  _ <- lexeme (TMC.char ']')

  _ <- lexeme (TMC.char ',')
  _ <- lexeme (string "probeSeq")
  _ <- lexeme (TMC.char '=')
  _ <- lexeme (TMC.char '[')
  probes <- parseProbeDescription `TM.sepBy` lexeme (TMC.char ',')
  _ <- lexeme (TMC.char ']')
  _ <- lexeme (TMC.char '}')

  return EvalExpected { postState = vals
                      , probeSeq = probes
                      }

-- | Return 'True' if the second description matches the first
matchingDesc :: LD.ProbeDescription -> LD.ProbeDescription -> Bool
matchingDesc tgt desc =
  and [ componentMatches LD.probeProvider
      , componentMatches LD.probeModule
      , componentMatches LD.probeFunction
      , componentMatches LD.probeName
      ]
  where
    -- Wildcards (on either side) match anything
    --
    -- NOTE: This might not be right in general; "real" probe descriptions might
    -- need to be fully specified, where a real probe description is provided by
    -- a platform.
    --
    -- Here we just need a loose matching to say that our probe code matches the
    -- probe trace in the test case.
    componentMatches a
      | T.null (a desc) || T.null (a tgt) = True
      | otherwise = a tgt == a desc

matchingProbeDescription :: LD.ProbeDescription
                         -> LDT.Probe globals
                         -> Bool
matchingProbeDescription desc (LDT.Probe descs _ _ _) =
  F.any (matchingDesc desc) descs

-- | Given a list of probes, run the one that matches the 'LD.ProbeDescription'
-- on the given global state
--
-- Fails with an assertion if there is no matching probe
evaluateProbes :: [LDT.Probe globals]
               -> LDTE.EvaluatorState globals
               -> LD.ProbeDescription
               -> IO (LDTE.EvaluatorState globals)
evaluateProbes probes s0 desc
  | Just p <- F.find (matchingProbeDescription desc) probes = return (LDTE.evalProbe p s0)
  | otherwise = TTH.assertFailure ("No probe for description: " ++ show desc)

checkVar :: Ctx.Assignment LDT.Variable globals
         -> Map.Map T.Text (Some (Ctx.Index globals))
         -> Ctx.Assignment LDTE.RegEntry globals
         -> (String, SomeReg)
         -> IO ()
checkVar globalVars globalMap s1 (varName, SomeReg exRep exVal) =
  case Map.lookup (T.pack varName) globalMap of
    Nothing -> TTH.assertFailure ("Missing index for variable " ++ show varName)
    Just (Some varIdx)
      | Just PC.Refl <- PC.testEquality (LDT.varRepr (globalVars Ctx.! varIdx)) exRep ->
          case (exRep, exVal, s1 Ctx.! varIdx) of
            (LDT.BoolRepr, LDTE.RegBool exBool, LDTE.RegBool actBool) ->
              TTH.assertEqual ("ExpectedBool for " ++ show varName) exBool actBool
            (LDT.StringRepr, LDTE.RegString exString, LDTE.RegString actString) ->
              TTH.assertEqual ("ExpectedString for " ++ show varName) exString actString
            (LDT.BVRepr _nr, LDTE.RegBV exBV, LDTE.RegBV actBV) ->
              TTH.assertEqual ("ExpectedBV for " ++ show varName) exBV actBV
            (LDT.FloatRepr LDT.SinglePrecRepr, LDTE.RegFloat exFloat, LDTE.RegFloat actFloat) ->
              TTH.assertEqual ("ExpectedFloat for " ++ show varName) exFloat actFloat
            (LDT.FloatRepr LDT.DoublePrecRepr, LDTE.RegDouble exDouble, LDTE.RegDouble actDouble) ->
              TTH.assertEqual ("ExpectedDouble for " ++ show varName) exDouble actDouble

      | otherwise -> TTH.assertFailure ("Unexpected type for variable: " ++ show varName)

mkEvalTest :: FilePath -> TT.TestTree
mkEvalTest expectedFile = TTH.testCase dfile $ do
  expectedText <- TIO.readFile expectedFile
  case TM.runParser parseEvalExpected expectedFile expectedText of
    Left err -> TTH.assertFailure ("Error parsing expected file: " ++ TM.errorBundlePretty err)
    Right ex -> do
      dcontent <- TIO.readFile dfile
      case LD.parseDTrace dfile dcontent of
        Left err -> TTH.assertFailure ("Error parsing DTrace file: " ++ show err)
        Right decls ->
          case LD.typeCheck decls of
            Left errs -> TTH.assertFailure ("Type errors: " ++ show errs)
            Right (LDT.Probes globalVars globalMap probes) -> do
              let s0 = LDTE.initialEvaluatorState globalVars
              LDTE.EvaluatorState s1 <- F.foldlM (evaluateProbes probes) s0 (probeSeq ex)
              F.forM_ (postState ex) (checkVar globalVars globalMap s1)
  where
    dfile = SF.dropExtension expectedFile
