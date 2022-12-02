#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, flow
-}
{-# language LambdaCase #-}

import Flow ((.>))
import Data.List.Extra (split)
import Data.List

main  = getContents >>= solve .> print

--solve :: String -> [Int]
solve input = (part <$> [1]) <*> (pure $ parse input)

parse :: String -> [[String]]
parse =  lines .> map words

--part :: Int -> [[Int]] -> Int
part 1 = map score .> sum
-- part 2 = id

data Out  = W | L | D -- win/loose/draw
data Move = R | P | S -- rock/paper/scissors
  deriving Eq

decode = \case
  "A" -> R
  "B" -> P
  "C" -> S
  "X" -> R
  "Y" -> P
  "Z" -> S

getOut R P = W
getOut P S = W
getOut S R = W
getOut m1 m2 | m1 == m2 = D
getOut _ _ = L

score [m1, m2] = scoreOut (getOut (decode m1) (decode m2)) +
  scoreMove (decode m2)

scoreMove = \case
  R -> 1
  P -> 2
  S -> 3

scoreOut :: Out -> Int
scoreOut = \case
  L -> 0
  D -> 3
  W -> 6
