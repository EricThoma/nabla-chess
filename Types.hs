module Types (
  BB
, MagicEntry (MagicEntry)
) where

import Data.Word
import Data.Array.Unboxed
import Criterion.Main
import Control.DeepSeq

-- bitboard
type BB = Word64

data MagicEntry = MagicEntry { mask :: BB
							 , magic :: BB
							 , shift :: Int
							 , lookup :: UArray Int BB
                  } deriving (Show)

-- used for my attempt at timing this code (sketchy)
instance NFData MagicEntry

