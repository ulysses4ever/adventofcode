#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, flow
-}
{-# language MultiWayIf #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra (chunksOf)
import Data.Char

main  = getContents >>= solve .> print

solve :: String -> [Int]
solve input = [ input |> parse1 |> part1
              , input |> parse2 |> part2
              ]

type Sack = (String, String)

parse1 :: String -> [Sack]
parse1 =  lines .> map (flip splitAt <*> (length .> (`div` 2)))

parse2 = lines

part1 = map (uncurry intersect .> head .> score) .> sum

part2 = chunksOf 3 .> map (foldl1' intersect .> head .> score) .> sum

score t = ord t + 1 + if
   | isLower t -> negate $ ord 'a'
   | otherwise -> 26 - ord 'A'
