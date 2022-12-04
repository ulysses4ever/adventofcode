#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, megaparsec
-}
{-# language LambdaCase #-}

import Flow ((.>))

main  = getContents >>= solve .> print

solve :: String -> [Int]
solve input = (part <$> [1,2]) <*> (pure $ parse input)

parse :: String -> [[String]]
parse =  lines .> id

-- part :: Int -> ? -> Int
part n = id -- map (score n) .> sum
