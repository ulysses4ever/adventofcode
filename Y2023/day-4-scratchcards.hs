module Main where

import AoC

import Data.List (intersect, foldl')

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> part1
    2 -> part2
  where
    wsmy' :: [([Int],[Int])]
    wsmy' = do
      wsmy <- inp
      let (ws, my) = span (/= '|') wsmy
      pure (tail $ input ws, input my)

    wsc = map (\(ws,my) -> length (ws `intersect` my)) wsmy'

    score wsc | wsc < 1 =  0
              | otherwise = 2^(wsc-1)
    part1 = map score wsc |> sum

    copies = fst $ foldl' f ([], repeat 1) wsc
      where
        f (rs, c:cs) w = (c:rs,
                          zipWith (+) cs (replicate w c ++ repeat 0))
    part2 = copies |> sum

main :: IO ()
main = defaultMain solve
