module Types (
  BB
, MagicEntry
, MagicEntry (MagicEntry)
) where

import Data.Word
import Data.Array.Unboxed

type BB = Word64

data MagicEntry = MagicEntry { mask :: BB
							 , magic :: BB
							 , shift :: Int
							 , lookup :: UArray Int BB
                  }

