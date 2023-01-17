module MCTrace.Runtime (
    SupportFunction(..)
  , supportFunctionNameMap
  , probeSupportFunctions
  , probeSupportFunctionIndexMap
) where


import           Data.Ix (Ix)
import qualified Data.Map as Map
import           Data.Tuple ( swap )

data SupportFunction = AllocMemory | Send | Timestamp
    deriving (Eq, Ord, Show, Enum, Ix)

supportFunctionNameMap :: Map.Map SupportFunction String
supportFunctionNameMap =
    Map.fromList [ (AllocMemory, "alloc_memory")
                 , (Send, "send")
                 , (Timestamp, "timestamp")
                 ]


probeSupportFunctions :: [SupportFunction]
probeSupportFunctions = [ Send, Timestamp ]

probeSupportFunctionIndexMap :: Map.Map SupportFunction Int
probeSupportFunctionIndexMap = Map.fromList $ zipWith (curry swap) [0..] probeSupportFunctions
