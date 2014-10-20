import Magic
import Types
import Utils
import System.Random as Rand
import Data.Array.Unboxed as UArray
import Data.Bits
import System.CPUTime
import Criterion.Main

main :: IO ()
main = let seed = 535345232
           (occ,gen) = sparseRand (Rand.mkStdGen $ fromIntegral seed)
       in do testMagic
             --print rookMagics
       	     --defaultMain [ bench "rook moves" $ nf (magicRookMoves occ) 32 ]
             
testMagic :: IO ()
testMagic = do seed <- getCPUTime
               let
                   (occ,gen) = sparseRand (Rand.mkStdGen $ fromIntegral seed)
                   mask = rookMasks UArray.! 36
               print64 occ
               print64 (magicRookMoves occ 36)

testOccs :: Int -> IO ()
testOccs sq = let mask = bishopMasks UArray.! sq
                  occs = genOccupancies mask sq
              in do print64 mask
                    sequence_ $ map print64 occs

testBishops :: BB -> Int -> IO()
testBishops bb i = if i < 64 
                   then do print64 bb
                           print $ linToRF i
                           print64 $ bishopMovesNoLookup bb i
                           testBishops bb (i+1)
	               else return ()