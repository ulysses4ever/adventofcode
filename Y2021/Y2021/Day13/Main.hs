module Main (main) where

import AoC

type In = ([P], [Int])

part :: Int -> String -> Int
part _p inp = traceShow inpStruct res
  where
    inpStruct :: In
    inpStruct = input inp
    res = 1

main :: IO ()
main = getContents >>= input .> part 1 .> print
