import Magic
import Types
import Utils

import Data.Array.Unboxed as UArray

main :: IO ()
main = do
	      let (r1:r2:rands) = randoms64 65437
	      --print64 r1
	      --print $ random64ToSquare r2
	      --print $ linToRF $ random64ToSquare r2
	      --print64 $ bishopMovesNoLookup r1 (random64ToSquare r2)
	      testOccs 0 --testBishops r1 0
	      {-let (r1:r2:rands) = ((54534::Int):(58::Int):[])--randoms64 65434
	      --print64 r1
	      print $ linToRF (((fromIntegral r2)::Int) `rem` 64)
	      print $ ((fromIntegral r2) `rem` 64)
	      print $ linToRF 58
	      print64 $ bishopMovesNoLookup (fromIntegral r1) ((fromIntegral r2) `rem` 64)-}

testOccs :: Int -> IO ()
testOccs sq = let mask = bishopMasks UArray.! sq
                  occs = genOccupancies mask sq
              in do print64 mask
                    sequence_ $ map print64 occs


genMagic bishop sq = if bishop 
                        then let mask = bishopMasks UArray.! sq
                                 occs = genOccupancies mask sq
                             in MagicEntry 0 0 0 (UArray.listArray (0,1) [])
                        else MagicEntry 0 0 0 (UArray.listArray (0,1) [])
testBishops :: BB -> Int -> IO()
testBishops bb i = if i < 64 
                   then do print64 bb
                           print $ linToRF i
                           print64 $ bishopMovesNoLookup bb i
                           testBishops bb (i+1)
	               else return ()