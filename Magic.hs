module Magic (
bishopMovesNoLookup,
genOccupancies,
bishopMasks,
genMagic,
magicBishopMoves,
magics
) where

import Types
import Utils
import Data.Array as Array
import Data.Array.Unboxed as UArray
import System.Random as Rand
import Data.Maybe
import Data.Bits
import Debug.Trace

magicBishopMoves :: BB -> Int -> BB
magicBishopMoves occ sq = 
                          let MagicEntry mask magic shift lookup = magics Array.! sq
                              om = occ .&. mask
                              opm = om * magic
                              ind = (fromIntegral $ opm `unsafeShiftR` shift)::Int
                          in lookup UArray.! ind

magics :: Array Int MagicEntry
magics = Array.listArray (0,63) [genMagic True i | i <- [0..63]]

magicsSquare :: Int -> MagicEntry
magicsSquare sq = let entry = randomEntry sq
				  in case entry of
				  	    Just x -> x
				  	    Nothing -> magicsSquare sq

genMagic :: Bool -> Int -> MagicEntry
genMagic bishop sq = let mask = if bishop
                                   then bishopMasks UArray.! sq
                                   else bishopMasks UArray.! sq
                         nbits = popCount mask
                         occs = genOccupancies mask sq
                         attmap = if bishop
                                     then (UArray.listArray (0,(bit nbits)-1) $
                                          map (\x -> bishopMovesNoLookup x sq) occs)::UArray Int BB
                                     else UArray.listArray (0,(bit nbits)-1) $ 
                                          map (\x -> bishopMovesNoLookup x sq) occs
                         (magic,result) = tryMagics (Rand.mkStdGen 2135632) attmap occs nbits
                     in MagicEntry mask magic (64-nbits) (UArray.array (0,(bit nbits)-1) result)
                        --then let mask = bishopMasks UArray.! sq
                        --else let mask = bishopMasks UArray.! sq

tryMagics :: StdGen -> UArray Int BB -> [BB] -> Int -> (BB,[(Int, BB)])
tryMagics gen attmap occs indexBits = let usedmap = (UArray.listArray (0,(bit indexBits)-1) (replicate (bit indexBits) False))::UArray Int Bool
                                          (magic, ngen) = sparseRand gen
                                          (res, success) = genMagicW attmap usedmap occs 0 indexBits magic []
                                      in if success
                                            then (magic,res)
                                            else tryMagics ngen attmap occs indexBits

genMagicW :: UArray Int BB -> UArray Int Bool -> [BB] -> Int -> Int -> BB -> [(Int,BB)] -> ([(Int, BB)], Bool)
genMagicW _ _ [] _ _ _ accum = (accum,True)
genMagicW attmap usedmap (occ:occs) occno indexBits magic accum =
          let index = (fromIntegral $ (occ*magic) `shiftR` (64-indexBits))::Int
              --index = trace (show $ UArray.indices attmap) (fromIntegral $ (occ*magic) `shiftR` (64-indexBits))::Int
          in if (usedmap UArray.! index)
                then (accum,False)
                else genMagicW attmap (usedmap UArray.// [(index,True)]) occs (occno+1) indexBits magic ((index, attmap UArray.! occno):accum)
                
                             
                        --else MagicEntry 0 0 0 (UArray.listArray (0,1) [])

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
                                        nocc = if (i `unsafeShiftR` j) .&. 1 /= 0
                                               then occ .|. (bit $ bitScanForward mask)
                                               else occ
                                    in getOcc nmask nbits i (j+1) nocc


randomEntry :: Int -> Maybe MagicEntry
randomEntry sq = Nothing

bishopMasks :: UArray Int BB
bishopMasks = UArray.listArray (0,63) [(0x007E7E7E7E7E7E00::BB) .&. (bishopMovesNoLookup 0 i) | i <- [0..63]]

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

