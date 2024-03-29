#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, vector
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Control.Monad.ST (runST)
import Data.List
import Data.Maybe
import qualified Data.Vector.Mutable as V
import Data.Vector (Vector, fromList, unsafeThaw, unsafeFreeze)
import qualified Data.Vector as VI
import Debug.Trace

-- element + flag if it has to be moved

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
part n inp = -- traceShow v'' $
  res n
  where
    res 1 = score v'
    res 2 = score v''

    score v = sum $ map (z v) [1000, 2000, 3000]
    key = 811589153 :: Int -- eger

    v' = VI.map snd $ mix v
      where
        v = fromList $ zip [0..] inp
    v'' = VI.map snd $ iterate mix v !! 10
      where
        v = fromList $ zip [0..] (map ((* key)) inp)

    zeroIdx v = VI.elemIndex 0 v |> fromJust
    z v n = v VI.! ((zeroIdx v + n) `mod` (VI.length v))

mix v = runST (unsafeThaw v' >>= go 0 >>= unsafeFreeze) |> VI.map fst
  where
    v' = VI.zip v $ VI.replicate len True -- at start, everyone needs a move
    go i' mv
      | i' == len = pure mv
      | otherwise = do
        let Just i = VI.findIndex (fst .> fst .> (== i')) v'
        ((_, x), inQueue) <- V.unsafeRead mv i
        let j = (i + x) `mod` (len - 1) -- (i + x + (x + i) `div` len) `mod` len
            width = abs (i - j)
            (startSrc, startTrg)
              | i <= j = (i+1, i)
              | otherwise = (j,j+1)
            src = V.slice startSrc width mv
            trg = V.slice startTrg width mv
        if inQueue
          then do
            -- trace ("Moving " ++ show x ++ " at index " ++ show i ++ " to index " ++ show j) (pure())
            -- v'' <- unsafeFreeze mv
            -- traceShow (VI.map (fst .> snd) v'') (pure ())
            V.move trg src
            V.unsafeWrite mv j ((i', x), False)
            go (if j < i then i'+1 else i') mv
          else
            go (i'+1) mv
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
parse :: String -> [Int]
parse =  lines .> map parseLine
