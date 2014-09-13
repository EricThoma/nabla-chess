import Magic
import Types
import Utils

main :: IO ()
main = do
	      let (r1:r2:rands) = randoms64 65437
	      print64 r1
	      print $ random64ToSquare r2
	      print $ linToRF $ random64ToSquare r2
	      print64 $ bishopMovesNoLookup r1 (random64ToSquare r2)

	      {-let (r1:r2:rands) = ((54534::Int):(58::Int):[])--randoms64 65434
	      --print64 r1
	      print $ linToRF (((fromIntegral r2)::Int) `rem` 64)
	      print $ ((fromIntegral r2) `rem` 64)
	      print $ linToRF 58
	      print64 $ bishopMovesNoLookup (fromIntegral r1) ((fromIntegral r2) `rem` 64)-}