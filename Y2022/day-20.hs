#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, vector, containers
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Control.Monad.ST (runST)
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed (Vector, fromList, unsafeThaw, unsafeFreeze)
import qualified Data.Vector.Unboxed as VI (length)
import Debug.Trace

type V = Vector Int

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n inp = seq (mix v) 1
  where
    v = fromList inp

-- v=fromList [1,2,3::Int]

mix v = runST (unsafeThaw v >>= go 0 >>= unsafeFreeze)
  where
    go i mv
      | i == len = traceShow v (pure mv)
      | otherwise = do
        x <- V.unsafeRead mv i
        let j = (i + x + len) `mod` len
            width = abs (i - j)
            (startSrc, startTrg)
              | i <= j = (i+1, i)
              | otherwise = (j,j+1)
            src = V.slice startSrc width mv
            trg = V.slice startTrg width mv
        traceShow v (pure ())
        V.move trg src
        V.unsafeWrite mv j x
        go (if j < i then i else i+1) mv
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
