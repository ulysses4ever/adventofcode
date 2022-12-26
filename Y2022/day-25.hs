#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.Maybe
import Data.Char

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n = foldl1' add

type Snafu = String

add :: Snafu -> Snafu -> Snafu
add x y = dropWhile (== '0') $  go (reverse x) (reverse y) '0' []
  where
    go [] [] carry res = carry:res
    go [] (y:ys) carry res = let (r,c) = addDigit y carry
      in go [] ys c (r:res)
    go lhs@(x:xs) [] carry res = go [] lhs carry res
    go (x:xs) (y:ys) carry res = let (r, carry') = addDigitCarry x y carry
      in go xs ys carry' (r:res)

addDigit x y = addDigitCarry x y '0'
addDigitCarry x y c = let
  n = num x + num y + num c
  an = abs n
  c' = signum n * if an > 2 then 1 else 0
  r = (n - 5 * c')
  in (unnum r, unnum c')

num = \case
  '-' -> (-1)
  '=' -> (-2)
  x   -> ord x - ord '0'

unnum = \case
  (-1) -> '-'
  (-2) -> '='
  x    -> chr $ x + ord '0'

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

{- Tests fir addDigit -}
-- |
-- >>> addDigit '0' '0'
-- ('0','0')
-- >>> addDigit '0' '1'
-- ('1','0')
-- >>> addDigit '1' '1'
-- ('2','0')
-- >>> addDigit '1' '2'
-- ('=','1')
-- >>> addDigit '2' '2'
-- ('-','1')
-- >>> addDigit '-' '0'
-- ('-','0')
-- >>> addDigit '=' '0'
-- ('=','0')
-- >>> addDigit '=' '-'
-- ('2','-')
-- >>> addDigit '=' '='
-- ('1','-')
