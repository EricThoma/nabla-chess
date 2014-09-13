module Magic (
magics,
bishopMovesNoLookup
) where

import Types
import Utils
import Data.Array as Array
import Data.Array.Unboxed as UArray
import Data.Maybe
import Data.Bits

magics :: Array Int MagicEntry
magics = Array.listArray (0,63) [magicsSquare i | i <- [0,63]]

magicsSquare :: Int -> MagicEntry
magicsSquare sq = let entry = randomEntry sq
				  in case entry of
				  	    Just x -> x
				  	    Nothing -> magicsSquare sq

randomEntry :: Int -> Maybe MagicEntry
randomEntry sq = Nothing

bishopMovesNoLookup :: BB -> Int -> BB
bishopMovesNoLookup occ sq = let (r, f) = linToRF sq
                                 ne = \(ri, fi) -> (ri+1, fi+1, ri+1<6 && fi+1<6 && (not $ testBit occ $ rfToLin (ri+1) (fi+1)))
                                 nw = \(ri, fi) -> (ri+1, fi-1, ri+1<6 && fi-1>1 && (not $ testBit occ $ rfToLin (ri+1) (fi-1)))
                                 se = \(ri, fi) -> (ri-1, fi+1, ri-1>1 && fi+1<6 && (not $ testBit occ $ rfToLin (ri-1) (fi+1)))
                                 sw = \(ri, fi) -> (ri-1, fi-1, ri-1>1 && fi-1>1 && (not $ testBit occ $ rfToLin (ri-1) (fi-1)))
                                 third = \(_,_,x) -> x
                             in      genWrapper ne (r, f, r+1<7 && f+1<7) 0
                                 .|. genWrapper nw (r, f, r+1<7 && f-1>0) 0
                                 .|. genWrapper se (r, f, r-1>0 && f+1<7) 0
                                 .|. genWrapper sw (r, f, r-1>0 && f-1>0) 0

genWrapper :: ((Int, Int) -> (Int, Int, Bool)) -> (Int, Int, Bool) -> BB -> BB
genWrapper func (_, _, False) att = att
genWrapper func (rank, file, True) att = let (rn, fn, b) = func (rank,file)
                                         in genWrapper func (rn,fn,b) (att .|. (bit $ rfToLin rn fn))

