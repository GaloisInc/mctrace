module MCTrace.Runtime (
    SupportFunction(..)
  , supportFunctionNameMap
  , probeSupportFunctions
  , probeSupportFunctionIndexMap
) where


import           Data.Ix (Ix)
import qualified Data.Map as Map
import           Data.Tuple ( swap )

data SupportFunction = AllocMemory | Send
    deriving (Eq, Ord, Show, Enum, Ix)

supportFunctionNameMap :: Map.Map SupportFunction String
supportFunctionNameMap =
    Map.fromList [ (AllocMemory, "alloc_memory")
                 , (Send, "send")
                 ]


probeSupportFunctions :: [SupportFunction]
probeSupportFunctions = [ Send ]

probeSupportFunctionIndexMap :: Map.Map SupportFunction Int
probeSupportFunctionIndexMap = Map.fromList $ zipWith (curry swap) [0..] probeSupportFunctions
