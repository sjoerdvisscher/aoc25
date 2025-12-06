import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Interval ((<=..<=), width)
import Data.IntervalSet ( Extended(Finite), fromList, toList )

main :: IO ()
main = interact (show . solve2 . parse)

parse :: String -> ([(Int, Int)], [Int])
parse s =
  let [ranges, ids] = splitOn "\n\n" s
  in (map (bimap read (read . drop 1) . break (== '-')) (lines ranges), map read (lines ids))

solve1 :: ([(Int, Int)], [Int]) -> Int
solve1 (ranges, ids) = length $ filter (\i -> any (\(l, h) -> l <= i && i <= h) ranges) ids

solve2 :: ([(Int, Int)], [Int]) -> Int
solve2 = sum . map (succ . width) . toList . fromList .  map (\(l, h) -> Finite l <=..<= Finite h) . fst