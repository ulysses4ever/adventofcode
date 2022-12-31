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
import qualified Data.Vector.Unboxed as VI
import Debug.Trace

-- element + flag if it has to be moved
type V = Vector (Int, Bool)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n inp = res n
  where
    res 1 = z 1000 + z 2000 + z 3000
    res 2 = 0

    v = fromList $ zip inp $ repeat True -- at start, everyone needs a move
    v' = VI.map fst $ mix v
    zeroIdx = VI.elemIndex 0 v' |> fromJust
    a <+> b = (a + b) `mod` (VI.length v')
    z n = v' VI.! (zeroIdx <+> n)

mix v = runST (unsafeThaw v >>= go 0 >>= unsafeFreeze)
  where
    go i mv
      | i == len = pure mv -- traceShow (VI.map fst v) (pure mv)
      | otherwise = do
        (x, inQueue) <- V.unsafeRead mv i
        let j = (i + x + (x + i) `div` len) `mod` len
            width = abs (i - j)
            (startSrc, startTrg)
              | i <= j = (i+1, i)
              | otherwise = (j,j+1)
            src = V.slice startSrc width mv
            trg = V.slice startTrg width mv
        if inQueue
          then do
            -- traceShow (VI.map fst v) (pure ())
            V.move trg src
            V.unsafeWrite mv j (x, False)
            go (if j < i then i+1 else i) mv
          else
            go (i+1) mv
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
