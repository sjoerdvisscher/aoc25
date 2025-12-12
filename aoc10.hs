import Data.Bits
import Data.List (transpose)
import Data.SBV
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace (traceShowId, traceShow)
import Data.Set qualified as Set

main :: IO ()
main = interact (show . sum . map (solve2' . parse . words) . lines)

parse :: [String] -> (Int, [[Int]], [Int])
parse ss = (parseTarget (head ss), map parseList (chop ss), parseList (last ss))
  where
    parseTarget = read . ("0b" ++) . reverse . map (\c -> if c == '#' then '1' else '0') . chop
    parseList = read . wrap . chop
    chop = tail . init
    wrap s = "[" ++ s ++ "]"

solve1 :: (Int, [[Int]], [Int]) -> Int
solve1 (target, buttons, _) = minimum [popCount i | i::Int <- [0..bit (length buttons) - 1], calc i == target]
  where
    calc i = getXor . foldMap (Xor . snd) . filter (testBit i . fst) . zip [0..] . map (sum . map bit) $ buttons

solve2 :: (Int, [[Int]], [Int]) -> Integer
solve2 (_, buttons, js) = traceShowId $ minimum $ map sum res
  where
    res :: [[Integer]] = unsafePerformIO $ extractModels <$> allSat constraints
    buttons' = map (\button -> map (\i -> literal $ if i `elem` button then 1::Integer else 0) [0..length js - 1]) buttons
    constraints = do
      xs <- mkFreeVars (length buttons)
      let rowEq row r = sum (zipWith (*) xs row) .== r
      solve $ zipWith rowEq (transpose buttons') (map (literal . fromIntegral) js) ++ map (.>= 0) xs

-- implementation of https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
solve2' :: (Int, [[Int]], [Int]) -> Int
solve2' (_, buttons, js) = traceShowId $ go (Set.singleton (0, js, 1))
  where
    go set = if all (== 0) r then c else go (set' <> Set.fromList opts)
      where
        ((c, r, f), set') = Set.deleteFindMin set
        opts = [ (c + popCount i * f, map (`div` 2) r', f * 2)
          | i::Int <- [0..bit (length buttons) - 1]
          , let bs = foldMap snd . filter (testBit i . fst) . zip [0..] $ buttons
          , let r' = map (\(k, j) -> j - length (filter (== k) bs)) (zip [0..] r)
          , all even r'
          , all (>= 0) r'
          ]