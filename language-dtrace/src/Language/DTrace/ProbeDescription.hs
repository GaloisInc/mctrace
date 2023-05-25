{-# LANGUAGE FlexibleContexts #-}
module Language.DTrace.ProbeDescription (
    ProbeDescription(..)
  , matchWithPattern

  , ProbeComponent(..)
  , mkPattern
  , mkIdentifier
  , mkFreePattern
  , appendIdentifier
  ) where

import qualified Data.Text as T
import qualified Text.Regex.TDFA as RE
import qualified Prettyprinter as PP

-- | A component of a probe description, used to match some aspect of a
-- function to be probed.
data ProbeComponent =
    FreePattern
    -- ^ A "free" pattern that trivially matches any identifier or
    -- context
    | Identifier T.Text
    -- ^ Match a specific identifier exactly
    | SymbolPattern T.Text RE.Regex
    -- ^ Match identifiers based on a regular expression. This carries
    -- the pre-compiled regex (the RE.Regex) and its original textual
    -- representation (the Text value).

instance Eq ProbeComponent where
    FreePattern == FreePattern = True
    Identifier a == Identifier b = a == b
    SymbolPattern p1 _ == SymbolPattern p2 _ = p1 == p2
    _ == _ = False

instance Show ProbeComponent where
    show FreePattern = "FreePattern"
    show (Identifier a) = "Identifier " <> show a
    show (SymbolPattern p _) = "SymbolPattern " <> show p <> " <regex>"

instance PP.Pretty ProbeComponent where
    pretty FreePattern = mempty
    pretty (Identifier i) = PP.pretty i
    pretty (SymbolPattern p _) = PP.pretty p

-- | A pattern matching a set of probes
data ProbeDescription =
  ProbeDescription { probeProvider :: ProbeComponent
                   , probeModule :: ProbeComponent
                   , probeFunction :: ProbeComponent
                   , probeName :: ProbeComponent
                   }
                   deriving (Eq, Show)

-- Constructors for ProbeComponent

-- | Given a Dtrace-style regular expression pattern, construct a
-- ProbeComponent from it.
mkPattern :: T.Text -> ProbeComponent
mkPattern p = SymbolPattern p (RE.makeRegex $ T.unpack $ convertToRegEx p)

-- | Construct a ProbeComponent to match the specified identifier.
mkIdentifier :: T.Text -> ProbeComponent
mkIdentifier = Identifier

-- | Construct a ProbeComponent for the free pattern.
mkFreePattern :: ProbeComponent
mkFreePattern = FreePattern

-- | Given a ProbeComponent, append the specified text to it if it
-- contains an Identifier and return the result; otherwise return
-- Nothing. This function might look strange but it's here partly to
-- help us avoid exporting constructors for ProbeComponent.
appendIdentifier :: ProbeComponent -> T.Text -> Maybe ProbeComponent
appendIdentifier (Identifier i) suffix = Just $ Identifier (i <> suffix)
appendIdentifier _ _ = Nothing

-- | Match a given text value against a ProbeComponent
matchWithPattern :: ProbeComponent -> T.Text -> Bool
matchWithPattern FreePattern _ = True
matchWithPattern (SymbolPattern _ pat) t = RE.matchTest pat t
matchWithPattern (Identifier i) t = i == t

-- | Convert a Dtrace regular expression pattern into one compatible
-- with RE.Regex
convertToRegEx :: T.Text -> T.Text
convertToRegEx pat = T.pack ('^' : convertToRegexImpl (T.unpack pat) ++ "$")
  where
    convertToRegexImpl "" = ""
    convertToRegexImpl ('*':t) = ".*" ++ convertToRegexImpl t
    convertToRegexImpl ('?':t) = '.' : convertToRegexImpl t
    convertToRegexImpl ('[':'!':c:t) = "[^" ++ c : charClass t
    convertToRegexImpl ('[':c:t)     = '['  :  c : charClass t
    convertToRegexImpl ('[':_)        = error "unterminated character class"
    convertToRegexImpl (c:t) = escape c ++ convertToRegexImpl t
    escape c | c `elem` "\\+()^$.{}]|" = '\\' : [c]
             | otherwise = [c]
    charClass (']':t) = ']' : convertToRegexImpl t
    charClass (c:t)   = c : charClass t
    charClass []       = error "unterminated character class"
