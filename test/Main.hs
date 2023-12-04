module Main where

import AoC

testParseInput y d = do
  i <- readFile ("test/input/Y" ++ show y ++ "/day-" ++ show d ++ ".txt")
  let !pi = parseInput y d i
  pPrint pi
  pure ()

parseInput =
  \cases
    2022 1 -> input @(HomoGroups [Int])
    _    _ -> error "unkwnown year/day pair"

main = do
  testParseInput 2022 1
