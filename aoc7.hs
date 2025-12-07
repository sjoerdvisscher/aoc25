import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Map qualified as Map

main :: IO ()
main = interact (show . solve2 . map parse . lines)

parse :: [Char] -> IntSet
parse = IS.fromList . map fst . filter ((/= '.') . snd) . zip [0..]

solve1 :: [IntSet] -> Int
solve1 [] = 0
solve1 (start:pss) = go start pss 0
  where
    go _ [] acc = acc
    go ps (sps:spss) acc = go ((ps IS.\\ hits) `IS.union` (IS.map pred hits) `IS.union` (IS.map succ hits)) spss (acc + IS.size hits)
      where
        hits = ps `IS.intersection` sps

solve2 :: [IntSet] -> Int
solve2 [] = 0
solve2 (start:pss) = go start' pss
  where
    start' = Map.fromList ((, 1::Int) <$> IS.toList start)
    go ps [] = sum (Map.elems ps)
    go ps (sps:spss) = go (Map.unionsWith (+) $ Map.mapWithKey (split sps) ps) spss
    split sps k v = Map.fromList $ if k `IS.member` sps then [(k - 1, v), (k + 1, v)] else [(k, v)]