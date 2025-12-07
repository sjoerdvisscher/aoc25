import Data.List (transpose)
import Data.List.Split (splitOn)

main :: IO ()
main = interact (show . sum . map calc . fix2 . lines)

fix1 :: [String] -> [(String, [String])]
fix1 = map (\ss -> (last ss, init ss)) . transpose . map words

fix2 :: [String] -> [(String, [String])]
fix2 ss = zip (words (last ss)) . map (map head) . splitOn [[]] . map words $ transpose (init ss)

calc :: (String, [String]) -> Int
calc (op, ss) = (if op == "+" then sum else product) (map read ss)
