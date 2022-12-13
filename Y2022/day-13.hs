#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, aeson, bytestring
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra (split)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Maybe (catMaybes)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n ps = zip ps rs
  where
    rs = map procPair ps

procPair :: [Value] -> Bool
procPair [Number n1, Number n2] = n1 < n2
procPair [v1@(Array _), v2@(Number _)] = procPair [v1, Array [v2]]
procPair [v1@(Number _), v2@(Array _)] = procPair [Array [v1], v2]
procPair [v1@(Array _), v2@(Array _)] = undefined
procPair _ = True

-- Read one line of problem's input into something more structured
parseGroup :: [String] -> [Value]
parseGroup = map (decode . BSC.pack) .> catMaybes

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = interact (solve .> map (map show) .> map unlines .> unlines)

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
solve input = (part <$> [1]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> split (== "") .> map parseGroup
