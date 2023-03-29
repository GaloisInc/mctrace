module Language.DTrace.ProbeDescription (
    ProbeDescription(..)
  , matchWithPattern
  , probeDescriptionMatches
  ) where

import qualified Data.Text as T
import qualified Text.Regex.TDFA as RE

-- | A pattern matching a set of probes
--
-- Each component is allowed to be the empty string (a totally free pattern)
data ProbeDescription =
  ProbeDescription { probeProvider :: T.Text
                   , probeModule :: T.Text
                   , probeFunction :: T.Text
                   , probeName :: T.Text
                   }
  deriving (Show, Eq)

-- | Test if two 'ProbeDescription's match
--
-- This accounts for all of the necessary wildcard logic (FIXME: wildcard
-- support is not yet complete, but it should all live here)
--
-- Probe descriptions match if each component matches
probeDescriptionMatches
  :: ProbeDescription
  -> ProbeDescription
  -> Bool
probeDescriptionMatches pd1 pd2 =
  and [ componentMatches probeProvider pd1 pd2
      , componentMatches probeModule pd1 pd2
      , componentMatches probeFunction pd1 pd2
      , componentMatches probeName pd1 pd2
      ]

-- | Probe components match if either is a wildcard (including empty) or if they
-- are equal
componentMatches :: (t -> T.Text) -> t -> t -> Bool
componentMatches f pd1 pd2 = matchWithPattern (f pd1) (f pd2)


matchWithPattern :: T.Text -> T.Text -> Bool
matchWithPattern pat t | T.null pat = True
                       | otherwise  = t RE.=~ convertToRegEx pat

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
