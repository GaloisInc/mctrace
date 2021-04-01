module Language.DTrace.ProbeDescription (
    ProbeDescription(..)
  , probeDescriptionMatches
  ) where

import qualified Data.Text as T

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
--
-- FIXME: a nearly full glob-style matching language is supported; that is not
-- entirely supported yet
componentMatches :: (t -> T.Text) -> t -> t -> Bool
componentMatches f pd1 pd2 =
  or [ T.null (f pd1)
     , T.null (f pd2)
     , T.pack "*" == f pd1
     , T.pack "*" == f pd2
     , f pd1 == f pd2
     ]
