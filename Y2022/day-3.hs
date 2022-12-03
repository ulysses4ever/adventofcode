#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, flow
-}
{-# language MultiWayIf #-}

import Flow ((.>))
import Data.List
import Data.Char

main  = getContents >>= solve .> print

solve :: String -> [Int]
solve input = (part <$> [1,2]) <*> (pure $ parse input)

type Sack = (String, String)

parse :: String -> [Sack]
parse =  lines .> map (flip splitAt <*> (length .> (`div` 2)))

part :: Int -> [Sack] -> Int
part n = map (score . head . uncurry intersect) .> sum

score t = ord t + 1 + if
   | isLower t -> negate $ ord 'a'
   | otherwise -> 26 - ord 'A'
