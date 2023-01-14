#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, search-algorithms, linear
-}
{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (|>))
import Data.List
import Algorithm.Search
import Linear.Vector
import Linear.V2
import Data.Maybe
import Data.Char
import Debug.Trace

type P = V2 Int
pattern P x y = V2 x y

type El = Char
type C = [[El]]

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part p inp = res
  where
    res = bfs
      next
      (\p -> get inp p == 'E')
      (initial inp)
      |> fromJust
      |> length
    next p = neighbors p
      |> filter inBounds
      |> filter (elevateOk p)
    elevateOk p p' = elev p' <= elev p + 1
    elev p = ord case get inp p of
          'S' -> 'a'
          'E' -> 'z'
          e   -> e
    (P rmx cmx) = size2 inp
    inBounds (P x y) = 0 <= x && x < rmx &&
      0 <= y && y < cmx

get :: C -> P -> El
get cont (P r c) = cont !! r !! c

initial :: C -> P
initial inp = P (length pre) (findIndex (== 'S') tar |> fromJust)
  where
    (pre, tar:post) = span ('S' `notElem`) inp

size2 :: [[a]] -> P
size2 m = P (length m) (length $ head m)

neighbors p = map (p +) basis ++ map (p -) basis

-- Read one line of problem's input into something more structured
-- parseLine :: String -> ???
parseLine = id

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
solve input = (part <$> [1,2]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine
