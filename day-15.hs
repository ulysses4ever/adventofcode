#!/usr/bin/env cabal
{- cabal:
build-depends: base, vector
-}
{-# language TypeApplications, RecordWildCards, StrictData, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -O2 #-}
module Main where

import Data.List (find)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

type Str = String

-- mapping: number -> turn id, the number was mentioned last
-- invariant: initialized entries are > 0, so initial value
--            for everything unseen is 0
type V = V.Vector Int

(!) :: V -> Int -> Int
(!) = V.unsafeIndex -- (V.!) --
(<~) :: V -> Int -> Int -> V
(<~) v i x = V.modify (\v' -> VM.write v' i x) v

final :: Int
final = 30000000 -- 2020

inp :: [Int]
inp = [6,3,15,13,1,0] -- [0,3,6] --

-- Part 1

data S = S {
  trn  :: Int, -- the turn counter
  lst  :: Int, -- last number seen
  lst0 :: Int, -- last turn we saw zero (premature optimization)
  mem  :: V } --,
  --trc  :: [Int] }
  deriving Show

initS :: S
initS = S l (last inp) last0' v' --(reverse inp)
  where
  l = length inp
  last0' = maybe 0 fst . find ((== 0) .snd) .
    reverse . zip [1..] $ inp
  v = V.replicate (final + 1) 0
  v' = V.update v (V.fromList $ zip inp [1..])

step :: S -> S
step S{..}
  | lst == 0 =
      S {lst  = lst' lst0,
         lst0 = trn,
         trn  = trn', ..}
  | otherwise =
      S {lst  = lst' (mem ! lst),
         mem = mem',
         trn  = trn', ..}
  where
  lst' 0 = 0
  lst' p = trn - p
  trn' = trn + 1
  mem' = mem <~ lst $ trn

-- Part 2

-- Main

main :: IO ()
main = do
  let res1 = lst $ iterate step initS !! (final - length inp) -- '-' -- foldl' procLine initS $ inp
  print res1
  let res2 = '-'
  print res2
  print "--"
