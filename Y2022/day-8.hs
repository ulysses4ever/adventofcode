#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>))
import Data.List
import Data.Word

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = sum (map pp i) + sum (map pp (transpose i))

p l = 1 + (length . takeWhile (\(a,b) -> a < b) $ zip l (tail l))

pp l = p l + p (reverse l)

-- Read one line of problem's input into something more structured
parseLine :: String -> [Word8]
parseLine = map (pure .> read)

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = interact (solve .> show)

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
solve input = (part <$> [1,2]) <*> (pure $ parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine
