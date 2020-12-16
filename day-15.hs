#!/usr/bin/env cabal
{- cabal:
build-depends: base, vector
-}
{-# language TypeApplications, RecordWildCards, StrictData, ViewPatterns, BlockArguments, NumericUnderscores #-}
{-# OPTIONS_GHC -Wall -O2 #-}
module Main where

import qualified Data.Vector.Unboxed as VP
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad

-----------------------------
--
--   Containers stuff: we use vector-based solution
--   (which is a bad idea, time for Part 2 is 20 sec)
--
-- mapping: number -> turn id, the number was mentioned last
-- invariant: initialized entries are > 0, so initial value
--            for everything unseen is 0
type V = V.IOVector Int
type M = IO

(!) :: V -> Int -> M Int
(!) v i
  | i >= V.length v = pure 0
  | otherwise       = V.unsafeRead v i

(<~) :: V -> Int -> Int -> M V
(<~) v i x = do
  v' <- if len <= i then g else pure v
  V.unsafeWrite v' i x
  pure v'
  where
  len = V.length v
  g = V.grow v $ i - len + 500

-------------------------------------------------
--
--  Constants
--

final :: Int
final = 30_000_000 -- 2020

inp :: [Int]
inp = [6,3,15,13,1,0] -- [0,3,6] --

initVectorSize :: Int
initVectorSize = maximum inp + 1

------------------------------------------------
--
-- Part 2

initMem :: M V
initMem = v'
  where
  v = VP.replicate initVectorSize 0
  v' = VP.thaw $ VP.update v (VP.fromList $ zip inp [1..])

step :: (V, Int) -> Int -> M (V, Int)
step (mem, lst) trn = do
    prev <- (mem ! lst)
    mem' <- mem <~ lst $ trn
    let next = if prev == 0 then 0 else trn - prev
    pure $ (mem', next)

-- Main

main :: IO ()
main = do
  mem <- initMem
  (_mem', res2) <- foldM step (mem, last inp)
    [length inp .. final - 1]
  print res2
