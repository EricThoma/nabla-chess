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
testMagic = do let seed = 50343 -- <- getCPUTime
               let
                   (opocc,gen) = sparseRand (Rand.mkStdGen $ fromIntegral seed)
                   (occ1,gen1) = sparseRand gen
                   selfocc = occ1 .&. (complement opocc)
                   occ = selfocc .|. opocc
                   mask = rookMasks UArray.! 28

               print64 occ
               print64 opocc
               print64 selfocc
               --print64 $ complement occ
               print64 $ magicQueenMoves opocc 28
               --print64 (genMove occ opocc 28)

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