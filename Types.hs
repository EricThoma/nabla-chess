module Types (
  BB
, MagicEntry (MagicEntry)
) where

import Data.Word
import Data.Array.Unboxed
import Criterion.Main
import Control.DeepSeq

type BB = Word64

data MagicEntry = MagicEntry { mask :: BB
							 , magic :: BB
							 , shift :: Int
							 , lookup :: UArray Int BB
                  } deriving (Show)

instance NFData MagicEntry

