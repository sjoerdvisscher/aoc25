import Data.List (transpose)
import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)

main :: IO ()
main = interact (show . sum . map calc . fix2 . lines)

fix1 :: [String] -> [[String]]
fix1 = transpose . map words

fix2 :: [String] -> [[String]]
fix2 ss = zipWith (\op args -> args ++ [op]) (words (last ss)) . map (map head) . splitOn [[]] . map words $ transpose (init ss)

calc :: [String] -> Int
calc ss = (if last ss == "+" then sum else product) (map read $ init ss)
