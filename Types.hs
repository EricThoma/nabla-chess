module Types (
  BB
, MagicEntry (MagicEntry)
) where

import Data.Word
import Data.Array.Unboxed

-- bitboard
type BB = Word64

data MagicEntry = MagicEntry { mask :: BB
							 , magic :: BB
							 , shift :: Int
							 , lookup :: UArray Int BB
                  } deriving (Show)

