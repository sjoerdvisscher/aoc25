main :: IO ()
main = interact (show . go 50 . map val . lines)

val :: String -> Integer
val (h:t) = (if h == 'L' then negate else id) $ read t

go :: Integer -> [Integer] -> Integer
go i = countZeros . scanl (\a b -> a + b) i

countZeros :: [Integer] -> Integer
countZeros xs = sum $ zipWith (\l r -> abs ((l `div` 100) - (r `div` 100)) + fixup l r) xs (drop 1 xs)

fixup :: Integer -> Integer -> Integer
fixup l r
  | l `mod` 100 == 0 && r < l = -1
  | r `mod` 100 == 0 && r < l = 1
  | otherwise = 0