#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, flow
-}

import Flow ((.>))
import Data.List.Extra (split)
import Data.List

main  = getContents >>= solve .> print

parse :: String -> [[Int]]
parse =  lines .> split (== "") .> map (map read)

part :: Int -> [[Int]] -> Int
part 1 = map sum .> maximum
part 2 = map sum .> sortOn negate .> take 3 .> sum

solve :: String -> [Int]
solve input = (part <$> [1,2]) <*> (pure $ parse input)

