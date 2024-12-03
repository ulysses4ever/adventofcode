module Main where

import AoC
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), (!?))

solve :: Map2D -> Int -> Int
solve (Map2D map') = \case
    1 -> part1
    2 -> part2
  where

    start = Map.foldlWithKey f Nothing map'
      where f acc pos char
              | Just _ <- acc = acc
              | char == 'S' = Just pos
              | otherwise = Nothing

    part1 = iterateWhile
      (\(isfirst, pos, _index) -> not isfirst && pos == start)
      (\(_, pos, index) -> let
          pos' = mconcat $ map tryStep []
          in (False, pos', index+1)
      )
      (True, start, 0)

    tryStep :: P -> Maybe P
    tryStep p = case map' !? p of
      Nothing -> Nothing
      Just pipe -> undefined

    part2 = 0

main :: IO ()
main = defaultMain solve
