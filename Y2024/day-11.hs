module Main (main) where

import AoC
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

type StoneCounts = IntMap Int

solve :: [Int] -> Int -> Int
solve inp = \case
    1 -> res' 25
    2 -> res' 75
  where
    res steps = iterate blink inp !! steps |> length
    res' steps = iterate blink' stoneCounts !! steps |> IntMap.foldl' (+) 0

    stoneCounts = counter inp

blink' :: StoneCounts -> StoneCounts
blink' = IntMap.foldlWithKey' f IntMap.empty
  where
    f accCounts stone count =
      foldl'
        (\accCounts' newStone -> IntMap.insertWith (+) newStone count accCounts')
        accCounts
        (updateStone stone)

-- This worked for part 1 but not part 2, which runs out of time
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
