main :: IO ()
main = interact (show . solve . map (\s -> read ("(" ++ s ++ ")")) . lines)

solve :: [(Integer, Integer)] -> Integer
solve ps = maximum [(x2 - x1 + 1) * (y2 - y1 + 1)
  | (xa, ya) <- ps, (xb, yb) <- ps, let x1 = min xa xb, let y1 = min ya yb, let x2 = max xa xb, let y2 = max ya yb
  , all (\(x, y) -> x <= x1 || x >= x2 || y <= y1 || y >= y2) testPoints]
  where
    ps' = ps ++ take 1 ps
    mids = zipWith (\(x1, y1) (x2, y2) -> ((x1 + x2) `div` 2, (y1 + y2) `div` 2)) ps' (drop 1 ps')
    testPoints = ps ++ mids
