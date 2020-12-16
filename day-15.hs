#!/usr/bin/env cabal
{- cabal:
build-depends: base, vector, pretty-simple
-}
{-# language TypeApplications, RecordWildCards, StrictData, ViewPatterns, BlockArguments, NumericUnderscores #-}
{-# OPTIONS_GHC -Wall -O2 -rtsopts #-}
module Main where

import Data.List (find)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
--import Text.Pretty.Simple
--import Debug.Trace
import Control.Monad.ST

type Str = String


-----------------------------
--
--   Containers stuff
--
-- mapping: number -> turn id, the number was mentioned last
-- invariant: initialized entries are > 0, so initial value
--            for everything unseen is 0
type V = V.Vector Int

{-
(!) :: V -> Int -> Int
(!) = V.unsafeIndex -- (V.!) --
(<~) :: V -> Int -> Int -> V
(<~) v i x = V.modify (\v' -> VM.write v' i x) v
-}


(!) :: V -> Int -> Int
(!) v i
  | i >= V.length v = 0
  | otherwise       = V.unsafeIndex v i -- (V.!) --
(<~) :: V -> Int -> Int -> V
(<~) v i x =
  if len <= i
    then g
    else w
  where
  len = V.length v
  g = runST do
    v' <- V.unsafeThaw v
    v'' <- VM.grow v' $ i - len + 500
    VM.write v'' i x
    V.unsafeFreeze v''
  w = (\mv -> VM.unsafeWrite mv i x) `V.modify` v

-------------------------------------------------

final :: Int
final = 60000 -- 30_000_000 -- 2020

inp :: [Int]
inp =  [6,3,15,13,1,0] --[0,3,6] --

initVectorSize :: Int
initVectorSize = 200

------------------------------------------------
--
-- Part 1

data S = S {
  trn  :: Int, -- the turn counter
  lst  :: Int, -- last number seen
  lst0 :: Int, -- last turn we saw zero (premature optimization)
  mem  :: V } --,
  --trc  :: [Int] }
  deriving Show

initS :: Int -> S
initS ivs = S l (last inp) last0' v' -- (reverse inp)
  where
  l = length inp
  last0' = maybe 0 fst . find ((== 0) .snd) .
    reverse . zip [1..] $ inp
  v = V.replicate (ivs + 1) 0
  v' = V.update v (V.fromList $ zip inp [1..])

step :: S -> S
step S{..}
  | lst == 0 =
      S {lst  = lst' lst0,
         lst0 = trn,
         trn  = trn',..}
         --trc=lst' lst0:trc,..}
  | otherwise =
      S {lst  = lst' (mem ! lst),
         mem = mem',
         trn  = trn',..}
         --trc=lst' lst0:trc, ..}
  where
  lst' 0 = 0
  lst' p = trn - p
  trn' = trn + 1
  mem' = mem <~ lst $ trn

part1 :: Int -> S
part1 ivs = iterate step (initS ivs) !! (final - length inp)

-- Main

main :: IO ()
main = do
  let its = part1 initVectorSize
      --itf = part1 final
      res1 = lst its -- (lst it, trc it)
      {-
      (s1,s2) = head . dropWhile
        (\(s1, s2) -> mem s1 == V.take (V.length $ mem s1) (mem s2)
          && (V.all (==0) $ V.drop (V.length $ mem s1) (mem s2))) $
        zip its itf
      -}
  print res1
  -- ~ print "--"
  -- ~ print $ part1 final
  --let res2 = '-'
  --print res2
  print "--"
