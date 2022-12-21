#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, vector
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Control.Monad.ST (runST)
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed (Vector, fromList, unsafeThaw, unsafeFreeze)
import qualified Data.Vector.Unboxed as VI (length)

type V = Vector Int

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n inp = length inp
  where
    v = fromList inp

mix v = runST (unsafeThaw v >>= go 0 >>= unsafeFreeze)
  where
    go len mv = pure mv
    go i mv = do
      x <- V.unsafeRead i mv
      -- let i' = if x > 0 then
      let j = (i + x + len) `mod` len
          -- s1 = V.slice (min i j) (abs (i - j + 1) mv
      pure mv
    len = VI.length v

-- Read one line of problem's input into something more structured
parseLine :: String -> Int
parseLine = read

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
