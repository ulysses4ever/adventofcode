module Main (main) where

import AoC

solve :: [[Int]] -> Int -> Int
solve inp = \case
    1 -> res ops1
    2 -> res ops2
  where
    res ops = map (maybeGood ops) inp |> catMaybes |> sum

ops1 = [(+), (*)]
ops2 = conc : ops1
conc x y = show x ++ show y |> read

maybeGood ops (target:xs)
  | target `elem` combinations target ops xs = Just target
  | otherwise = Nothing

combinations target ops (x:xs) = foldl' f [x] xs
  where
    f ys x = concat $ foldl' (\acc op -> filter (<= target) (map (`op` x) ys) : acc) [] ops

main :: IO ()
main = defaultMain solve
