module Main (main) where

import AoC

solve :: [Int] -> Int -> Int
solve inp = \case
    1 -> res 25
    2 -> 0--res 75
  where
    res steps = iterate blink inp !! steps |> length

blink :: [Int] -> [Int]
blink = concatMap updateStone

updateStone :: Int -> [Int]
updateStone 0 = [1]
updateStone n
  | even len = [read l, read r]
  | otherwise = [n*2024]
  where
    ns = show n
    len = length ns
    (l, r) = splitAt (len `div` 2) ns

main :: IO ()
main = defaultMain solve
