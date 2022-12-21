#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, containers
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.Maybe
import qualified Data.Map as M

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n inp = prog M.! "root"
  where
    prog = M.fromList $ map interp inp
    interp = \case
      [v, n] -> (init v, read n :: Int)
      [v, opd1, op, opd2] -> (init v, (parseOp op) (prog M.! opd1) (prog M.! opd2))
    parseOp = \case
      "+" -> (+)
      "-" -> (-)
      "*" -> (*)
      "/" -> (div)

-- Read one line of problem's input into something more structured
-- parseLine :: String -> ???
parseLine = words

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
