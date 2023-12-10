{-# language CPP #-}
module Main where

import Data.List ( sort, group, sortOn, sortBy )

import AoC
import Data.Char (isDigit, ord)
import Data.Ord

#define p2

data C =
#ifndef p2
    C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | J | Q | K | A
#else
    J | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | T | Q | K | A
#endif
    deriving (Eq, Ord, Enum, Show, Read)

readCard :: Char -> C
readCard c
  | isDigit c = iterate succ C2 !! (ord c - ord '2')
  | otherwise = read [c]

type H = [C]

readHand :: String -> H
readHand = map readCard

solve :: [String] -> Int -> Int
solve inp p = case p of
    1 -> part1
    2 -> part2
  where
    hsbs :: [((H, S), Int)]
    hsbs = map (words .> \[h, b] -> let h' = readHand h in ((h', sch h'), read b)) inp
    sch | p == 1 = scoreHand
        | otherwise = scoreHand'
    res
      =  hsbs |> sortBy (\(h1,_) (h2,_) -> cmpHands h1 h2)
      .> zip [1..]
      .> map (\(i,(_,b)) -> i*b)
      .> sum

    part1 = res
    part2 = res

data S =
    HH | P1 | P2 | K3 | FH | K4 | K5
    deriving (Eq, Ord, Enum, Show, Read)

scoreHand :: [C] -> S
scoreHand h = case h |> sort .> group .> sortOn length of
    [a] -> K5
    [a,b] -> case length a of
      1 -> K4
      2 -> FH
    [a,b,c] -> case length c of
      3 -> K3
      2 -> P2
    [a,b,c,d] -> P1
    _ -> HH

-- assumes hand is sorted
scoreHand' :: H -> S
scoreHand' = sort .> variate .> map scoreHand .> maximum

variate :: H -> [H]
variate h = go h
  where
    go h' = case h' of
        []   -> [[]]
        J:h'' -> h >>= (\c -> map (c:) $ variate h'')
        _    -> pure h

cmpHands :: (H, S) -> (H, S) -> Ordering
cmpHands (h1, s1) (h2, s2)
  | h1 == h2 = EQ
  | s1 == s2 = compare h1 h2
  | otherwise = compare s1 s2

main :: IO ()
main = defaultMain solve
-}
