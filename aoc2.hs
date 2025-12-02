import Data.List.Split (splitOn)

main :: IO ()
main = interact (show . sum . map count . parse)

parse :: String -> [(Integer, Integer)]
parse = map ((\[a,b] -> (read a, read b)) . splitOn "-") . splitOn ","

count :: (Integer, Integer) -> Integer
count (l, h) = sum [n | n <- [l..h], test2 (show n)]

test1 :: String -> Bool
test1 s = let (a, b) = splitAt (length s `div` 2) s in a == b

test2 :: String -> Bool
test2 s = let l = length s in
  or [ take l (cycle (take i s)) == s | i <- [1..l `div` 2], l `mod` i == 0]
