module MCTrace.Runtime ( 
    SupportFunction(..)
  , SupportFunctionMap
  , supportFunctionNameMap
) where

import qualified Data.Map as Map
import Data.Ix (Ix)


data SupportFunction = AllocMemory | Send
    deriving (Eq, Ord, Show, Enum, Ix)

type SupportFunctionMap = Map.Map SupportFunction

supportFunctionNameMap :: SupportFunctionMap String
supportFunctionNameMap = 
    Map.fromList [ (AllocMemory, "alloc_memory")
                 , (Send, "send")
                 ]

