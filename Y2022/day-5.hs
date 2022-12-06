#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, ilist
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.Function
import Data.List
import Data.List.Extra (replace)
import Data.List.Index (setAt)
import Data.Char

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n (state, moves) =
  map head $ foldl' (makeMove n) state moves

makeMove p s [n, from, to] =
  setAt to' t' (setAt from' f' s)
  where
    from' = from - 1
    to' = to - 1
    f = s !! from'
    t = s !! to'
    (m, f') = splitAt n f
    t' = (if p == 1 then reverse m else m) ++ t

-- Turn problem's full text into something more structured
parse i = (state3, moves2)
  where
    (state1, empty:moves1) = i |> lines |> span (/= "")
    state2 = state1 |> map (replace "    " " [x]") |> map (filter isAlpha)
    state3 = state2 |> transpose |> map (filter (/= 'x'))

    moves2 = moves1 |> map
        (groupBy ((==) `on` isDigit)
         .> filter (isDigit . head)
         .> map read)

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = getContents >>= solve .> print

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
--solve :: String -> [Int]
solve input = (part <$> [1,2]) <*> (pure $ parse input)
