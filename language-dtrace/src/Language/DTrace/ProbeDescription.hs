module Language.DTrace.ProbeDescription (
  ProbeDescription(..)
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
