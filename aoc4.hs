main :: IO ()
main = interact (show . val . lines)

val :: [String] -> Int
val lns0 = sum (map (length . filter (== 'x')) (fixed lns0))
  where
    go lns = [[ if c == '@' && p < 4 then 'x' else c
              | x <- [0..h-1]
              , let c = lns !! y !! x
              , let p::Int = sum [ 1 | dx <- [-1..1], dy <- [-1..1]
                                     , dx /= 0 || dy /= 0
                                     , let xx = x + dx, let yy = y + dy
                                     , xx >= 0, xx < w
                                     , yy >= 0, yy < h
                                     , lns !! yy !! xx == '@' ]]
             | y <- [0..w-1]]
    fixed lns = let lns' = go lns in if lns' == lns then lns else fixed lns'
    w = length (lns0 !! 0)
    h = length lns0
