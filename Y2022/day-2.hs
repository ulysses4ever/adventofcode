#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow
-}
{-# language LambdaCase #-}

import Flow ((.>))
import Data.List

main  = getContents >>= solve .> print

solve :: String -> [Int]
solve input = (part <$> [1,2]) <*> (pure $ parse input)

parse :: String -> [[String]]
parse =  lines .> map words

part :: Int -> [[String]] -> Int
part n = map (score n) .> sum

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

decode2 = \case
  "X" -> L
  "Y" -> D
  "Z" -> W

getOut R P = W
getOut P S = W
getOut S R = W
getOut m1 m2 | m1 == m2 = D
getOut _ _ = L

getMove R W = P
getMove P W = S
getMove S W = R
getMove m1 D = m1
getMove R L = S
getMove P L = R
getMove S L = P

score 1 [m1, m2] = scoreOut (getOut (decode m1) (decode m2)) +
  scoreMove (decode m2)
score 2 [m1, m2] = scoreMove (getMove (decode m1) (decode2 m2)) +
  scoreOut (decode2 m2)

scoreMove = \case
  R -> 1
  P -> 2
  S -> 3

scoreOut :: Out -> Int
scoreOut = \case
  L -> 0
  D -> 3
  W -> 6
