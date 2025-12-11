import Data.Map qualified as Map
import Data.List (uncons)
import Data.Bifunctor (first)
import Data.Maybe (catMaybes)

main :: IO ()
main = interact (show . solve2 . parse)

parse :: String -> Map.Map String [String]
parse = Map.fromList . catMaybes . map (fmap (first init) . uncons . words) . lines

solve1 :: Map.Map String [String] -> Int
solve1 m = counts Map.! "you"
  where
    counts = fmap (sum . map (counts Map.!)) m `Map.union` Map.singleton "out" 1

solve2 :: Map.Map String [String] -> Int
solve2 m = counts "fft" "svr" * counts "dac" "fft" * counts "out" "dac"
  where
    counts tgt = f where
      c = fmap (sum . map f) m
      f d = if d == tgt then 1 else Map.findWithDefault 0 d c
