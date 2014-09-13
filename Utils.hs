module Utils (print64, randoms64, linToRF, rfToLin, random64ToSquare) where

import Types
import Data.Bits
import System.Random as Rand

linToRF :: Int -> (Int, Int)
linToRF sq = quotRem sq 8--(sq `quot` 8, sq `rem` 8)

rfToLin :: Int -> Int -> Int
rfToLin rank file = rank*8 + file

print64 :: BB -> IO ()
print64 b = let l64 = replicate 64 b
                shifts = 1:(take 63 (map (shift 1) [1..])) :: [BB]
                inter = zipWith (.&.) shifts l64
                result = (map fromIntegral) $ zipWith shift inter (map (*(-1))[0..63]) :: [Int]
            in printLikeABoardIterate 0 result


printLikeABoardIterate :: Int -> [Int] -> IO ()
printLikeABoardIterate 64 _ = putStr "\n"
printLikeABoardIterate i xs = do let realIndex = \a -> 64 - 8*( truncate ((fromIntegral (a+8))/8) ) + (a `rem` 8) :: Int
                                 if i `rem` 8 == 0
                                     then putStr $ "\n"
                                     else return ()
                                 let x = (xs !! (realIndex i))
                                 putStr $ ( show x ) ++ " "
                                 printLikeABoardIterate (i+1) xs

randoms64 :: Int -> [BB]
randoms64 = fromRandoms . Rand.randoms . mkStdGen

random64ToSquare :: BB -> Int
random64ToSquare r = abs(fromIntegral r) `rem` 64

fromRandoms :: [Int] -> [BB]
fromRandoms (a:b:c:xs) = (    (fromIntegral a) 
	                      .|. (shiftL (fromIntegral b) 30)
                          .|. (shiftL (fromIntegral c) 60) ):(fromRandoms xs)
