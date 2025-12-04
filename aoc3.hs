import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = interact (show . sum . map (solve . map (read . pure)) . lines)

solve :: [Int] -> Int
solve = go 12 0
  where
    go 0 acc _ = acc
    go n acc bs = let m = maximum (take (length bs - n + 1) bs) in
      go (n - 1) (acc * 10 + m) (drop (fromJust (elemIndex m bs) + 1) bs)