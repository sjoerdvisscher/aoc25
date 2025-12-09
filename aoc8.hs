import Data.List (sortOn, find, delete)

main :: IO ()
main = interact (show . solve . map (\s -> read ("(" ++ s ++ ")")) . lines)

solve :: [(Int, Int, Int)] -> Int
solve pts = go (map (pure . fst) ixd) pairs 1000000
  where
    ixd = zip [(0::Int)..] pts
    pairs = map snd $ sortOn fst [ (d2 p1 p2, (i1, i2)) | (i1, p1) <- ixd, (i2, p2) <- ixd, i1 < i2 ]
    go grps _ 0 = product $ take 3 $ map length $ sortOn (negate . length) grps
    go grps ((i1, i2):ps) n = case (find (i1 `elem`) grps, find (i2 `elem`) grps) of
      (Just g1, Just g2) | g1 /= g2 ->
        if length grps == 2
          then case (pts !! i1, pts !! i2) of ((x1, _, _), (x2, _, _)) -> x1 * x2
          else go ((g1 ++ g2) : delete g1 (delete g2 grps)) ps (n - 1)
      _ -> go grps ps (n - 1)
    d2 (x1, y1, z1) (x2, y2, z2) = sqr (x1 - x2) + sqr (y1 - y2) + sqr (z1 - z2)
    sqr x = x * x