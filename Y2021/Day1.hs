{-# language BangPatterns #-}
module Day1 (solve1, solve2)

solve1 :: String -> IO ()
solve1 = undefined

countInc :: [Int] -> Int
countInc xs = foldl1' (((x, x'), !s) -> if x < x' then s + 1 else s) (zip xs tail xs)

solve2 = undefined

