module Magic (
magics,
bishopMovesNoLookup,
genOccupancies,
bishopMasks
) where

import Types
import Utils
import Data.Array as Array
import Data.Array.Unboxed as UArray
--import Data.Array.Diff as DiffArray
import Data.Maybe
import Data.Bits


magics :: Array Int MagicEntry
magics = Array.listArray (0,63) [magicsSquare i | i <- [0,63]]

magicsSquare :: Int -> MagicEntry
magicsSquare sq = let entry = randomEntry sq
				  in case entry of
				  	    Just x -> x
				  	    Nothing -> magicsSquare sq

genMagic :: Bool -> Int -> MagicEntry
genMagic bishop sq = if bishop 
                        then let mask = bishopMasks UArray.! sq
                                 occs = genOccupancies mask sq
                             in MagicEntry 0 0 0 (UArray.listArray (0,1) [])
                        else MagicEntry 0 0 0 (UArray.listArray (0,1) [])

genOccupancies :: BB -> Int -> [BB]
genOccupancies mask sq = let nbits = popCount mask
                         in genOccupanciesW mask nbits sq 0 []

genOccupanciesW :: BB -> Int -> Int -> Int -> [BB] -> [BB]
genOccupanciesW mask nbits sq iter accum = if iter >= (bit nbits)
                                              then accum
                                              else genOccupanciesW mask nbits sq (iter+1) ((getOcc mask nbits iter 0 0):accum)
    

getOcc :: BB -> Int -> Int -> Int -> BB -> BB
getOcc mask nbits i j occ = if j >= nbits
                               then occ
                               else let nmask = mask .&. (mask-1)
                                        nocc = if (i `shiftR` j) .&. 1 /= 0
                                               then occ .|. (bit $ bitScanForward mask)
                                               else occ
                                    in getOcc nmask nbits i (j+1) nocc


randomEntry :: Int -> Maybe MagicEntry
randomEntry sq = Nothing

bishopMasks :: UArray Int BB
bishopMasks = UArray.listArray (0,63) [(0x007E7E7E7E7E7E00::BB) .&. (bishopMovesNoLookup 0 i) | i <- [0,63]]

bishopMovesNoLookup :: BB -> Int -> BB
bishopMovesNoLookup occ sq = let (r, f) = linToRF sq
                                 ne = \(ri, fi) -> (ri+1, fi+1, ri+1<7 && fi+1<7 && (not $ testBit occ $ rfToLin (ri+1) (fi+1)))
                                 nw = \(ri, fi) -> (ri+1, fi-1, ri+1<7 && fi-1>0 && (not $ testBit occ $ rfToLin (ri+1) (fi-1)))
                                 se = \(ri, fi) -> (ri-1, fi+1, ri-1>0 && fi+1<7 && (not $ testBit occ $ rfToLin (ri-1) (fi+1)))
                                 sw = \(ri, fi) -> (ri-1, fi-1, ri-1>0 && fi-1>0 && (not $ testBit occ $ rfToLin (ri-1) (fi-1)))
                                 third = \(_,_,x) -> x
                             in      genWrapper ne (r, f, r<7 && f<7) 0
                                 .|. genWrapper nw (r, f, r<7 && f>0) 0
                                 .|. genWrapper se (r, f, r>0 && f<7) 0
                                 .|. genWrapper sw (r, f, r>0 && f>0) 0

genWrapper :: ((Int, Int) -> (Int, Int, Bool)) -> (Int, Int, Bool) -> BB -> BB
genWrapper func (_, _, False) att = att
genWrapper func (rank, file, True) att = let (rn, fn, b) = func (rank,file)
                                         in genWrapper func (rn,fn,b) (att .|. (bit $ rfToLin rn fn))

