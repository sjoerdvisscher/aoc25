import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.Split (splitOn)

main :: IO ()
main = interact (show . solve . parse)

type V2 = (Int, Int)
parse :: String -> ([Set V2], [(V2, [Int])])
parse s = (map (parseShape . tail) (init parts), map parseRegion (last parts))
  where
    parts = splitOn [""] $ lines s
    parseShape sh = Set.fromList [ (x, y) | (y, l) <- zip [0..] sh, (x , c) <- zip [0..] l, c == '#']
    parseRegion r = ((read w, read h), map read cs)
      where
        (d:cs) = words r
        [w,h] = splitOn ("x") (init d)

solve :: ([Set V2], [(V2, [Int])]) -> Int
solve (shapes, regions) = sum (map (\(d, cs) -> fromEnum $ solve1 (zip shapes cs) d) regions)

solve1 :: [(Set V2, Int)] -> V2 -> Bool
solve1 shapes (w, h) = w * h >= sum (map (\(sh, c) -> c * Set.size sh) shapes)
